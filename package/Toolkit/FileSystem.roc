## A collection of prebuilt tools for interacting with the file system. For safety reasons, the tools in this module are limited to working in the current working directory and its subdirectories.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [listDirectory, listFileTree, readFileContents, writeFileContents ]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [
##     (listDirectory.name, listDirectory.handler),
##     (listFileTree.name, listFileTree.handler),
##     (readFileContents.name, readFileContents.handler),
##     (writeFileContents.name, writeFileContents.handler),
## ]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages 
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { pathFromStr, pathToStr, listDir!, isDir!, readFile!, writeUtf8! } -> [
    listDirectory,
    #listDirectoryHandler!,
    listFileTree,
    readFileContents,
    writeFileContents,
]

import json.Json
import InternalTools exposing [Tool, buildTool]

## Expose name, handler and tool for listDirectory.
listDirectory : { name : Str, handler! : Str => Result Str *, tool : Tool }
listDirectory = {
    name: listDirectoryTool.function.name,
    handler!: listDirectoryHandler!,
    tool: listDirectoryTool,
}

## Tool definition for the listDirectory function
listDirectoryTool : Tool
listDirectoryTool =
    pathParam = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a directory. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    buildTool "listDirectory" "List the contents of a directory" [pathParam]

## Handler for the listDirectory tool
listDirectoryHandler! : Str => Result Str _
listDirectoryHandler! = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Ok "Failed to decode args"

        Ok { path } ->
            if path |> Str.contains ".." then
                Ok "Invalid path: `..` is not allowed"
            else if path |> Str.startsWith "/" then
                Ok "Invalid path: must be a relative path"
            else
                listDir! (pathFromStr path)
                |> Result.withDefault []
                |> List.map pathToStr
                |> Str.joinWith "\n"
                |> Ok

## Expose name, handler and tool for listFileTree.
##
## This tool will allow the model to list the contents of a directory, and all subdirectories.
listFileTree : { name : Str, handler! : Str => Result Str *, tool : Tool }
listFileTree = {
    name: listFileTreeTool.function.name,
    handler!: listFileTreeHandler!,
    tool: listFileTreeTool,
}

## Tool definition for the listFileTree function
listFileTreeTool : Tool
listFileTreeTool =
    pathParam = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a directory. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    buildTool "listFileTree" "List the contents of a directory and all subdirectories" [pathParam]

## Handler for the listFileTree tool
listFileTreeHandler! : Str => Result Str _
listFileTreeHandler! = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Ok "Failed to decode args"

        Ok { path } ->
            if path |> Str.contains ".." then
                Ok "Invalid path: `..` is not allowed"
            else if path |> Str.startsWith "/" then
                Ok "Invalid path: must be a relative path"
            else
                dirContents = path |> pathFromStr |> listDir! |> Result.withDefault []
                fileTreeHelper! dirContents "" 0

## Recursive helper function for listFileTreeHandler
fileTreeHelper! : List path, Str, U64 => Result Str _
fileTreeHelper! = \paths, accumulation, depth ->
    prependNewline = \str -> if Str.isEmpty str then str else Str.concat "\n" str
    appendNewline = \str -> if Str.isEmpty str then str else Str.concat str "\n"
    buildStr = \previous, current, subcontents -> "$(appendNewline previous)$(current)$(subcontents)"

    when paths is
        [] ->
            Ok accumulation

        [path, .. as pathsTail] ->
            if pathToStr path |> Str.contains "/." then
                fileTreeHelper! pathsTail accumulation depth
            else if try isDir! path then
                subcontents = try fileTreeHelper! (try listDir! path) "" (depth + 1) |> prependNewline
                newString = buildStr accumulation (pathToStr path) subcontents
                fileTreeHelper! pathsTail newString depth
            else
                newString = buildStr accumulation (pathToStr path) ""
                fileTreeHelper! pathsTail newString depth

## Expose name, handler and tool for readFileContents.
##
## This tool will allow the model to read the contents of a file.
readFileContents : { name : Str, handler! : Str => Result Str *, tool : Tool }
readFileContents = {
    name: readFileContentsTool.function.name,
    handler!: readFileContentsHandler!,
    tool: readFileContentsTool,
}

## Tool definition for the readFileContents function
readFileContentsTool : Tool
readFileContentsTool =
    pathParam = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a directory. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    buildTool "readFileContents" "Read the contents of a file. Must be a plain text file (any extension)." [pathParam]

## Handler for the readFileContents tool
readFileContentsHandler! : Str => Result Str _
readFileContentsHandler! = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Ok "Failed to decode args"

        Ok { path } ->
            if path |> Str.contains ".." then
                Ok "Invalid path: `..` is not allowed"
            else if path |> Str.startsWith "/" then
                Ok "Invalid path: must be a relative path"
            else
                path
                |> pathFromStr
                |> readFile!
                |> Result.withDefault "Failed to read file"
                |> Ok

## Expose name, handler and tool for writeFileContents.
##
## This tool will allow the model to write content to a file.
writeFileContents : { name : Str, handler! : Str => Result Str *, tool : Tool }
writeFileContents = {
    name: writeFileContentsTool.function.name,
    handler!: writeFileContentsHandler!,
    tool: writeFileContentsTool,
}

## Tool definition for the writeFileContents function
writeFileContentsTool : Tool
writeFileContentsTool =
    pathParam = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a file. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    contentParam = {
        name: "content",
        type: "string",
        description: "The full text content to write to the file. This must be the full content of the file.",
        required: Bool.true,
    }
    buildTool
        "writeFileContents"
        """
        Write the text content to a file. Any existing file at the specified path will be overwritten. If the file does not exist, it will be created, but parent directories must exist.
        """
        [pathParam, contentParam]

## Handler for the writeFileContents tool
writeFileContentsHandler! : Str => Result Str _
writeFileContentsHandler! = \args ->
    decoded : Decode.DecodeResult { path : Str, content : Str }
    decoded = args |> Str.toUtf8 |> Decode.fromBytesPartial Json.utf8
    when decoded.result is
        Err _ ->
            Ok "Failed to decode args"

        Ok { path, content } ->
            if path |> Str.contains ".." then
                Ok "Invalid path: `..` is not allowed"
            else if path |> Str.startsWith "/" then
                Ok "Invalid path: must be a relative path"
            else
                path
                |> pathFromStr
                |> writeUtf8! content
                |> Result.try \_ -> Ok "File successfully updated."
                |> Result.onErr handleWriteErr
                |> Result.withDefault "Error writing to file"
                |> Ok

handleWriteErr = \err ->
    when err is
        FileWriteErr _ NotFound -> Ok "File not found"
        FileWriteErr _ AlreadyExists -> Ok "File already exists"
        FileWriteErr _ Interrupted -> Ok "Write interrupted"
        FileWriteErr _ OutOfMemory -> Ok "Out of memory"
        FileWriteErr _ PermissionDenied -> Ok "Permission denied"
        FileWriteErr _ TimedOut -> Ok "Timed out"
        FileWriteErr _ WriteZero -> Ok "Write zero"
        FileWriteErr _ (Other str) -> Ok str
