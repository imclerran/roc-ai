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
module { path_from_str, path_to_str, list_dir, is_dir, read_file, write_utf8 } -> [
    list_directory,
    list_file_tree,
    read_file_contents,
    write_file_contents,
]

import json.Json
import InternalTools exposing [Tool, build_tool]

## Expose name, handler and tool for listDirectory.
list_directory : { name : Str, handler : Str -> Task Str *, tool : Tool }
list_directory = {
    name: list_directory_tool.function.name,
    handler: list_directory_handler,
    tool: list_directory_tool,
}

## Tool definition for the listDirectory function
list_directory_tool : Tool
list_directory_tool =
    path_param = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a directory. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    build_tool("list_directory", "List the contents of a directory", [path_param])

## Handler for the listDirectory tool
list_directory_handler : Str -> Task Str _
list_directory_handler = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Task.ok ("Failed to decode args")

        Ok({ path }) ->
            if path |> Str.contains("..") then
                Task.ok ("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Task.ok ("Invalid path: must be a relative path")
            else
                list_dir(path_from_str(path))
                |> Task.result!
                |> Result.with_default([])
                |> List.map(path_to_str)
                |> Str.join_with("\n")
                |> Task.ok

## Expose name, handler and tool for listFileTree.
##
## This tool will allow the model to list the contents of a directory, and all subdirectories.
list_file_tree : { name : Str, handler : Str -> Task Str *, tool : Tool }
list_file_tree = {
    name: list_file_tree_tool.function.name,
    handler: list_file_tree_handler,
    tool: list_file_tree_tool,
}

## Tool definition for the listFileTree function
list_file_tree_tool : Tool
list_file_tree_tool =
    path_param = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a directory. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    build_tool("list_file_tree", "List the contents of a directory and all subdirectories", [path_param])

## Handler for the listFileTree tool
list_file_tree_handler : Str -> Task Str _
list_file_tree_handler = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Task.ok ("Failed to decode args")

        Ok({ path }) ->
            if path |> Str.contains("..") then
                Task.ok ("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Task.ok ("Invalid path: must be a relative path")
            else
                dir_contents = path |> path_from_str |> list_dir |> Task.result! |> Result.with_default([])
                file_tree_helper(dir_contents, "", 0)

## Recursive helper function for listFileTreeHandler
file_tree_helper : List path, Str, U64 -> Task Str _
file_tree_helper = \paths, accumulation, depth ->
    prepend_newline = \str -> if Str.is_empty(str) then str else Str.concat("\n", str)
    append_newline = \str -> if Str.is_empty(str) then str else Str.concat(str, "\n")
    build_str = \previous, current, subcontents -> "$(append_newline(previous))$(current)$(subcontents)"

    when paths is
        [] ->
            Task.ok(accumulation)

        [path, .. as pathsTail] ->
            if path_to_str(path) |> Str.contains("/.") then
                file_tree_helper(paths_tail, accumulation, depth)
            else if is_dir!(path) then
                subcontents = file_tree_helper!(list_dir!(path), "", (depth + 1)) |> prepend_newline
                new_string = build_str(accumulation, path_to_str(path), subcontents)
                file_tree_helper(paths_tail, new_string, depth)
            else
                new_string = build_str(accumulation, path_to_str(path), "")
                file_tree_helper(paths_tail, new_string, depth)

## Expose name, handler and tool for readFileContents.
##
## This tool will allow the model to read the contents of a file.
read_file_contents : { name : Str, handler : Str -> Task Str *, tool : Tool }
read_file_contents = {
    name: read_file_contents_tool.function.name,
    handler: read_file_contents_handler,
    tool: read_file_contents_tool,
}

## Tool definition for the readFileContents function
read_file_contents_tool : Tool
read_file_contents_tool =
    path_param = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a directory. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    build_tool("read_file_contents", "Read the contents of a file. Must be a plain text file (any extension).", [path_param])

## Handler for the readFileContents tool
read_file_contents_handler : Str -> Task Str _
read_file_contents_handler = \args ->
    decoded : Decode.DecodeResult { path : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Task.ok ("Failed to decode args")

        Ok({ path }) ->
            if path |> Str.contains("..") then
                Task.ok ("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Task.ok ("Invalid path: must be a relative path")
            else
                path
                |> path_from_str
                |> read_file
                |> Task.result!
                |> Result.with_default ("Failed to read file")
                |> Task.ok

## Expose name, handler and tool for writeFileContents.
##
## This tool will allow the model to write content to a file.
write_file_contents : { name : Str, handler : Str -> Task Str *, tool : Tool }
write_file_contents = {
    name: write_file_contents_tool.function.name,
    handler: write_file_contents_handler,
    tool: write_file_contents_tool,
}

## Tool definition for the writeFileContents function
write_file_contents_tool : Tool
write_file_contents_tool =
    path_param = {
        name: "path",
        type: "string",
        description: "The relative unix style path to a file. `..` is not allowed. Must begin with `.`",
        required: Bool.true,
    }
    content_param = {
        name: "content",
        type: "string",
        description: "The full text content to write to the file. This must be the full content of the file.",
        required: Bool.true,
    }
    build_tool(
        "write_file_contents",
        """
        Write the text content to a file. Any existing file at the specified path will be overwritten.
        If the file does not exist, it will be created, but parent directories must exist.
        """,
        [path_param, content_param],
    )

## Handler for the writeFileContents tool
write_file_contents_handler : Str -> Task Str _
write_file_contents_handler = \args ->
    decoded : Decode.DecodeResult { path : Str, content : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Task.ok ("Failed to decode args")

        Ok({ path, content }) ->
            if path |> Str.contains("..") then
                Task.ok ("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Task.ok ("Invalid path: must be a relative path")
            else
                path
                |> path_from_str
                |> write_utf8(content)
                |> Task.result!
                |> Result.try(\_ -> Ok("File successfully updated."))
                |> Result.on_err(handle_write_err)
                |> Result.with_default ("Error writing to file")
                |> Task.ok

handle_write_err = \err ->
    when err is
        FileWriteErr(_, NotFound) -> Ok("File not found")
        FileWriteErr(_, AlreadyExists) -> Ok("File already exists")
        FileWriteErr(_, Interrupted) -> Ok("Write interrupted")
        FileWriteErr(_, OutOfMemory) -> Ok("Out of memory")
        FileWriteErr(_, PermissionDenied) -> Ok("Permission denied")
        FileWriteErr(_, TimedOut) -> Ok("Timed out")
        FileWriteErr(_, WriteZero) -> Ok("Write zero")
        FileWriteErr(_, Other(str)) -> Ok(str)
