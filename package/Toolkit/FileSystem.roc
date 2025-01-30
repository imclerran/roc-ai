## A collection of prebuilt tools for interacting with the file system. For safety reasons, the tools in this module are limited to working in the current working directory and its subdirectories.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [
##     list_directory.tool, 
##     list_file_tree.tool, 
##     read_file_contents.tool, 
##     write_file_contents.tool,
## ]
## # Tool handler map is passed to Tools.handle_tool_calls!
## tool_handler_map = Dict.from_list([
##     (list_directory.name, listDirectory.handler),
##     (list_file_tree.name, list_file_tree.handler),
##     (read_file_contents.name, read_file_contents.handler),
##     (write_file_contents.name, write_file_contents.handler),
## ])
## client = Client.new { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.add_user_message(client, newMessage, {})
## response = Http.send!(Chat.build_http_request(client, {}))?
## with_tool_results = 
##      Chat.update_messages(response, messages)?
##     |> Tools.handle_tool_calls!(client toolHandlerMap, { max_model_calls: 5 })
## ```
module { path_from_str, path_to_str, list_dir!, is_dir!, read_file!, write_utf8! } -> [
    list_directory,
    list_file_tree,
    read_file_contents,
    write_file_contents,
]

import json.Json
import InternalTools exposing [Tool, build_tool]

## Expose name, handler and tool for listDirectory.
list_directory : { name : Str, handler! : Str => Result Str _, tool : Tool }
list_directory = {
    name: list_directory_tool.function.name,
    handler!: list_directory_handler!,
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
list_directory_handler! : Str => Result Str _
list_directory_handler! = |args|
    decoded : Decode.DecodeResult { path : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ path }) ->
            if path |> Str.contains("..") then
                Ok("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Ok("Invalid path: must be a relative path")
            else
                list_dir!(path_from_str(path))
                |> Result.with_default([])
                |> List.map(path_to_str)
                |> Str.join_with("\n")
                |> Ok

## Expose name, handler and tool for listFileTree.
##
## This tool will allow the model to list the contents of a directory, and all subdirectories.
list_file_tree : { name : Str, handler! : Str => Result Str _, tool : Tool }
list_file_tree = {
    name: list_file_tree_tool.function.name,
    handler!: list_file_tree_handler!,
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
list_file_tree_handler! : Str => Result Str _
list_file_tree_handler! = |args|
    decoded : Decode.DecodeResult { path : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ path }) ->
            if path |> Str.contains("..") then
                Ok("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Ok("Invalid path: must be a relative path")
            else
                dir_contents = path |> path_from_str |> list_dir! |> Result.with_default([])
                file_tree_helper!(dir_contents, "", 0)

## Recursive helper function for listFileTreeHandler
file_tree_helper! : List _, Str, U64 => Result Str _
file_tree_helper! = |paths, accumulation, depth|
    prepend_newline = |str| if Str.is_empty(str) then str else Str.concat("\n", str)
    append_newline = |str| if Str.is_empty(str) then str else Str.concat(str, "\n")
    build_str = |previous, current, subcontents| "${append_newline(previous)}${current}${subcontents}"

    when paths is
        [] ->
            Ok(accumulation)

        [path, .. as paths_tail] ->
            if path_to_str(path) |> Str.contains("/.") then
                file_tree_helper!(paths_tail, accumulation, depth)
            else if try is_dir!(path) then
                subcontents = try file_tree_helper!(try list_dir!(path), "", (depth + 1)) |> prepend_newline
                new_string = build_str(accumulation, path_to_str(path), subcontents)
                file_tree_helper!(paths_tail, new_string, depth)
            else
                new_string = build_str(accumulation, path_to_str(path), "")
                file_tree_helper!(paths_tail, new_string, depth)

## Expose name, handler and tool for readFileContents.
##
## This tool will allow the model to read the contents of a file.
read_file_contents : { name : Str, handler! : Str => Result Str _, tool : Tool }
read_file_contents = {
    name: read_file_contents_tool.function.name,
    handler!: read_file_contents_handler!,
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
read_file_contents_handler! : Str => Result Str _
read_file_contents_handler! = |args|
    decoded : Decode.DecodeResult { path : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ path }) ->
            if path |> Str.contains("..") then
                Ok("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Ok("Invalid path: must be a relative path")
            else
                path
                |> path_from_str
                |> read_file!
                |> Result.with_default("Failed to read file")
                |> Ok

## Expose name, handler and tool for writeFileContents.
##
## This tool will allow the model to write content to a file.
write_file_contents : { name : Str, handler! : Str => Result Str _, tool : Tool }
write_file_contents = {
    name: write_file_contents_tool.function.name,
    handler!: write_file_contents_handler!,
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
        "Write the text content to a file. Any existing file at the specified path will be overwritten. If the file does not exist, it will be created, but parent directories must exist.",
        [path_param, content_param],
    )

## Handler for the writeFileContents tool
write_file_contents_handler! : Str => Result Str _
write_file_contents_handler! = |args|
    decoded : Decode.DecodeResult { path : Str, content : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ path, content }) ->
            if path |> Str.contains("..") then
                Ok("Invalid path: `..` is not allowed")
            else if path |> Str.starts_with("/") then
                Ok("Invalid path: must be a relative path")
            else
                # path
                # |> path_from_str
                content
                |> write_utf8!(path_from_str(path))
                |> Result.try(|_| Ok("File successfully updated."))
                |> Result.on_err(handle_write_err)
                |> Result.with_default("Error writing to file")
                |> Ok

handle_write_err = |err|
    when err is
        FileWriteErr(_, NotFound) -> Ok("File not found")
        FileWriteErr(_, PermissionDenied) -> Ok("Permission denied")
        FileWriteErr(_, BrokenPipe) -> Ok("Broken pipe")
        FileWriteErr(_, AlreadyExists) -> Ok("File already exists")
        FileWriteErr(_, Interrupted) -> Ok("Write interrupted")
        FileWriteErr(_, Unsupported) -> Ok("Unsupported operation")
        FileWriteErr(_, OutOfMemory) -> Ok("Out of memory")
        FileWriteErr(_, Other(str)) -> Ok(str)

        # FileWriteErr(_, TimedOut) -> Ok("Timed out")
        # FileWriteErr(_, WriteZero) -> Ok("Write zero")
