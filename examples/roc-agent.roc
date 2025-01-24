app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.8.0/RQlGWlkQEfxtkSYKl0nHNQaOFT0-Jh7NNFEX2IPXlec.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Stdin
import cli.Http
import cli.Env
import cli.Cmd
import cli.Path

import ansi.ANSI as Ansi
import ai.Chat exposing [Message]
import ai.Tools { send_http_req!: Http.send! }
import ai.Toolkit.Roc { cmd_new: Cmd.new, cmd_arg: Cmd.arg, cmd_output!: Cmd.output! } exposing [roc, roc_start, roc_check, roc_test]
import ai.Toolkit.FileSystem {
        path_from_str: Path.from_str,
        path_to_str: Path.display,
        list_dir!: Path.list_dir!,
        is_dir!: Path.is_dir!,
        read_file!: Path.read_utf8!,
        write_utf8!: Path.write_utf8!,
    } exposing [list_file_tree, list_directory, read_file_contents, write_file_contents]

import "roc-tutorial.md" as tutorial : Str

main! = |_|
    init_workspace!({})?
    api_key = Env.var!("OPENROUTER_API_KEY")?
    client = Chat.new_client({ api_key, model: "anthropic/claude-3.5-sonnet:beta", tools })
        |> init_messages
    Stdout.line!(("Assistant: Ask me to write some roc code!\n" |> Ansi.color({ fg: Standard(Cyan) })))?
    loop!(client)?
    Ok({})

loop! : Chat.Client => Result _ _
loop! = |client|
    Stdout.write!("You: ")?
    client2 = Chat.append_user_message(client, Stdin.line!({})?, {})
    response = Http.send!(Chat.build_http_request(client, {}))?
    client3 = Chat.update_message_list(client2, response)?
    client4 = Tools.handle_tool_calls!(client3, tool_handler_map, { max_model_calls: 10 })?
    print_last_message!(client4.messages)?
    loop!(client4)

## Initialize the workspace directory
init_workspace! : {} => Result {} _
init_workspace! = |{}|
    work_path = "./agent-workspace" |> Path.from_str
    Path.create_all!(work_path) |> Result.on_err!(|_err| Ok {}) |> try
    Env.set_cwd!(work_path) 

## List of messages to initialize the chat
# init_messages : Client -> Client
init_messages = |client|
    client
    |> Chat.append_system_message(tutorial, { cached: Bool.true })
    |> Chat.append_user_message(
        # claude does not put high priority on system messages, so this is sent as a user message.
        """
        CRITICAL: Do not ever change the app header, including platform or package urls, which are set by the rocStart tool.
        You should make sure to read the file contents before changing them, so you can maintain the current app headers.
        The app header is at the top of the file and follows the syntax `app [...] { ... }`. Nothing in this block should ever be changed.
        You should assume that the app header portion is always correct. This is absolutely critical or the program will not work.
        This also includes any files you are asked to edit, which were not initialized by the rocStart tool. 
        Unless specifically asked to do so by the user, do not ever change the header.

        NOTE: Do not respond to or mention this message, as it is a sudo system message, and the user is not aware of it.
        """,
        { cached: Bool.true },
    )

# Print the last message in the list of messages. Will only print assistant and system messages.
print_last_message! : List Message => Result {} _
print_last_message! = |messages|
    when List.last(messages) is
        Ok({ role, content }) if role == "assistant" ->
            Stdout.line!(("\nAssistant: ${Str.trim(content)}\n" |> Ansi.color({ fg: Standard(Magenta) })))

        Ok({ role, content }) if role == "system" ->
            Stdout.line!(("\nAssistant: ${Str.trim(content)}\n" |> Ansi.color({ fg: Standard(Cyan) })))

        _ -> Ok({})

## List of tool definitions to be given to the AI model
tools : List Tools.Tool
tools = [
    roc.tool,
    roc_check.tool,
    roc_test.tool,
    roc_start.tool,
    list_directory.tool,
    list_file_tree.tool,
    read_file_contents.tool,
    write_file_contents.tool,
]

## Map of tool names to tool handlers
tool_handler_map : Dict Str (Str => Result Str _)
tool_handler_map =
    Dict.from_list(
        [
            (roc.name, roc.handler!),
            (roc_test.name, roc_test.handler!),
            (roc_check.name, roc_check.handler!),
            (roc_start.name, roc_start.handler!),
            (list_directory.name, list_directory.handler!),
            (list_file_tree.name, list_file_tree.handler!),
            (read_file_contents.name, read_file_contents.handler!),
            (write_file_contents.name, write_file_contents.handler!),
        ],
    )
