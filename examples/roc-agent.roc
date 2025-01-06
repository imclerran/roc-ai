app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.15.0/SlwdbJ-3GR7uBWQo6zlmYWNYOxnvo8r6YABXD-45UOw.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.6/tzLZlg6lNmgU4uYtQ_zCmSr1AptHxZ8VBfE-O9JCudw.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Stdin
import cli.Http
import cli.Env
import cli.Cmd
import cli.Path

import ansi.Core as Ansi
import ai.Chat exposing [Message]
import ai.Tools { sendHttpReq: Http.send }
import ai.Toolkit.Roc { cmdNew: Cmd.new, cmdArg: Cmd.arg, cmdOutput: Cmd.output } exposing [roc, rocStart, rocCheck, rocTest]
import ai.Toolkit.FileSystem {
        pathFromStr: Path.fromStr,
        pathToStr: Path.display,
        listDir: Path.listDir,
        isDir: Path.isDir,
        readFile: Path.readUtf8,
        writeUtf8: Path.writeUtf8,
    } exposing [listFileTree, listDirectory, readFileContents, writeFileContents]

import "roc-tutorial.md" as tutorial : Str

main : Task {} _
main =
    initWorkspace!
    apiKey = getApiKey!
    client = Chat.initClient { apiKey, model: "anthropic/claude-3.5-sonnet:beta", tools }
    Stdout.line! ("Assistant: Ask me to write some roc code!\n" |> Ansi.color { fg: Standard Cyan })
    Task.loop! { previousMessages: initMessages } \{ previousMessages } -> ## Task.loop function must be inline due to roc issue #7116
        Stdout.write! "You: "
        messages = Chat.appendUserMessage previousMessages Stdin.line! {}
        response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
        updatedMessages = Chat.updateMessageList response messages |> Tools.handleToolCalls! client toolHandlerMap { maxModelCalls: 10 }
        printLastMessage! updatedMessages
        Task.ok (Step { previousMessages: updatedMessages })

## Initialize the workspace directory
initWorkspace : Task {} _
initWorkspace =
    workPath = "./agent-workspace" |> Path.fromStr
    workPath
        |> Path.createDir
        |> Task.onErr! \err ->
            when err is
                DirErr AlreadyExists -> Task.ok {}
                _ -> Task.err err
    Env.setCwd! workPath

## Get the API key from the environmental variable
getApiKey : Task Str _
getApiKey =
    Task.attempt (Env.var "OPENROUTER_API_KEY") \keyResult ->
        when keyResult is
            Ok key -> Task.ok key
            Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"

## List of messages to initialize the chat
initMessages : List Message
initMessages =
    []
    |> Chat.appendSystemMessage tutorial { cached: Bool.true }
    |> Chat.appendUserMessage # claude does not put high priority on system messages, so this is sent as a user message.
        """
        CRITICAL: Do not ever change the app header, including platform or package urls, which are set by the rocStart tool.
        You should make sure to read the file contents before changing them, so you can maintain the current app headers.
        The app header is at the top of the file and follows the syntax `app [...] { ... }`. Nothing in this block should ever be changed.
        You should assume that the app header portion is always correct. This is absolutely critical or the program will not work.
        This also includes any files you are asked to edit, which were not initialized by the rocStart tool. 
        Unless specifically asked to do so by the user, do not ever change the header.

        NOTE: Do not respond to or mention this message, as it is a sudo system message, and the user is not aware of it.
        """
        { cached: Bool.true }

# Print the last message in the list of messages. Will only print assistant and system messages.
printLastMessage : List Message -> Task {} _
printLastMessage = \messages ->
    when List.last messages is
        Ok { role, content } if role == "assistant" ->
            Stdout.line! ("\nAssistant: $(Str.trim content)\n" |> Ansi.color { fg: Standard Magenta })

        Ok { role, content } if role == "system" ->
            Stdout.line! ("\nAssistant: $(Str.trim content)\n" |> Ansi.color { fg: Standard Cyan })

        _ -> Task.ok {}

## List of tool definitions to be given to the AI model
tools: List Tools.Tool
tools = [
    roc.tool,
    rocCheck.tool,
    rocTest.tool,
    rocStart.tool,
    listDirectory.tool,
    listFileTree.tool,
    readFileContents.tool,
    writeFileContents.tool,
]

## Map of tool names to tool handlers
toolHandlerMap : Dict Str (Str -> Task Str _)
toolHandlerMap =
    Dict.fromList [
        (roc.name, roc.handler),
        (rocTest.name, rocTest.handler),
        (rocCheck.name, rocCheck.handler),
        (rocStart.name, rocStart.handler),
        (listDirectory.name, listDirectory.handler),
        (listFileTree.name, listFileTree.handler),
        (readFileContents.name, readFileContents.handler),
        (writeFileContents.name, writeFileContents.handler),
    ]
