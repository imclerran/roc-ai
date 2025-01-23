## A collection of prebuilt tools for interacting with the Roc programming language CLI.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [roc, rocCheck, rocTest, rocStart]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [
##     (roc.name, roc.handler),
##     (rocCheck.name, rocCheck.handler),
##     (rocTest.name, rocTest.handler),
##     (rocStart.name, rocStart.handler),
## ]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = try Http.send (Chat.buildHttpRequest client messages {})
## updatedMessages = updateMessagesFromResponse response messages
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { cmd_new, cmd_arg, cmd_output! } -> [roc, roc_check, roc_test, roc_start]

import json.Json
import InternalTools exposing [Tool, build_tool]

## Expose name, handler and tool for roc.
##
## This tool will allow the model to run `roc` for a roc file.
roc : { name : Str, handler! : Str => Result Str *, tool : Tool }
roc = {
    name: roc_tool.function.name,
    handler!: roc_handler!,
    tool: roc_tool,
}

roc_tool : Tool
roc_tool =
    roc_file_param = {
        name: "rocFile",
        type: "string",
        description: "The path to the .roc file to be executed. IE: `./path/to/file.roc`",
        required: Bool.true,
    }
    build_tool("roc", "Build a roc application from a .roc file, and run it if there are no errors.", [roc_file_param])

roc_handler! : Str => Result Str _
roc_handler! = |args|
    decoded : Decode.DecodeResult { roc_file : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Ok("Failed to decode args")

        Ok({ roc_file }) ->
            if roc_file |> Str.contains("..") then
                Ok("Invalid path: `..` is not allowed")
            else if roc_file |> Str.starts_with("/") then
                Ok("Invalid path: must be a relative path")
            else
                cmd_new("roc")
                |> cmd_arg(roc_file)
                |> cmd_output!
                |> cmd_output_to_str
                |> Ok

## Expose name, handler and tool for rocCheck.
##
## This tool will allow the model to run `roc check` for a Roc file.
roc_check : { name : Str, handler! : Str => Result Str *, tool : Tool }
roc_check = {
    name: roc_check_tool.function.name,
    handler!: roc_check_handler!,
    tool: roc_check_tool,
}

roc_check_tool : Tool
roc_check_tool =
    roc_file_param = {
        name: "rocFile",
        type: "string",
        description: "The path to the .roc file to be executed. IE: `./path/to/file.roc`",
        required: Bool.true,
    }
    build_tool("rocCheck", "Check a Roc file for syntax errors", [roc_file_param])

roc_check_handler! : Str => Result Str _
roc_check_handler! = |args|
    decoded : Decode.DecodeResult { roc_file : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Ok("Failed to decode args")

        Ok({ roc_file }) ->
            if roc_file |> Str.contains("..") then
                Ok("Invalid path: `..` is not allowed")
            else if roc_file |> Str.starts_with("/") then
                Ok("Invalid path: must be a relative path")
            else
                cmd_new("roc")
                |> cmd_arg("check")
                |> cmd_arg(roc_file)
                |> cmd_output!
                |> cmd_output_to_str
                |> Ok

## Expose name, handler and tool for rocTest.
##
## This tool will allow the model to run `roc test` for a Roc file.
roc_test : { name : Str, handler! : Str => Result Str *, tool : Tool }
roc_test = {
    name: roc_test_tool.function.name,
    handler!: roc_test_handler!,
    tool: roc_test_tool,
}

roc_test_tool : Tool
roc_test_tool =
    roc_file_param = {
        name: "rocFile",
        type: "string",
        description: "The path to the .roc file to be tested. IE: `./path/to/file.roc`",
        required: Bool.true,
    }
    build_tool("rocTest", "Test the expect statements in a specified roc file.", [roc_file_param])

roc_test_handler! : Str => Result Str _
roc_test_handler! = |args|
    decoded : Decode.DecodeResult { roc_file : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Ok("Failed to decode args")

        Ok({ roc_file }) ->
            if roc_file |> Str.contains("..") then
                Ok("Invalid path: `..` is not allowed")
            else if roc_file |> Str.starts_with("/") then
                Ok("Invalid path: must be a relative path")
            else
                cmd_new("roc")
                |> cmd_arg("test")
                |> cmd_arg(roc_file)
                |> cmd_output!
                |> cmd_output_to_str
                |> Ok

## Expose name, handler and tool for rocStart.
##
## This tool will allow the model to use `roc-start` to initialize a new Roc application.
roc_start : { name : Str, handler! : Str => Result Str *, tool : Tool }
roc_start = {
    name: roc_start_tool.function.name,
    handler!: roc_start_handler!,
    tool: roc_start_tool,
}

roc_start_tool : Tool
roc_start_tool =
    app_name_param = {
        name: "appName",
        type: "string",
        description: "The name of the .roc application to be initialized. IE: `myApp` will generate `myApp.roc`",
        required: Bool.true,
    }
    platform_param = {
        name: "platform",
        type: "string",
        description: "The platform to use in the new roc application. May be one of: `basic-cli`, or `basic-webserer`",
        required: Bool.true,
    }
    build_tool(
        "rocStart",
        """
        Start a new Roc application with the specified name and platform. 
        You should always use this tool when creating a new roc program,
        and make sure to read read the generated output file before changing
        it, so that the correct package and platform urls can be maintained.
        """,
        [app_name_param, platform_param],
    )

roc_start_handler! : Str => Result Str _
roc_start_handler! = |args|
    decoded : Decode.DecodeResult { app_name : Str, platform : Str }
    decoded = args |> Str.to_utf8 |> Decode.from_bytes_partial(Json.utf8)
    when decoded.result is
        Err(_) ->
            Ok("Failed to decode args")

        Ok({ app_name, platform }) ->
            if app_name |> Str.contains("..") then
                Ok("Invalid appName: `..` is not allowed")
            else if app_name |> Str.contains("/") then
                Ok("Invalid appName: name may not be a path")
            else if app_name |> Str.contains(".roc") then
                Ok("Invalid appName: name may not contain file extension.")
            else if platform != "basic-cli" and platform != "basic-webserver" then
                Ok("Invalid platform: must be one of 'basic-cli' or 'basic-webserver'")
            else
                cmd_new("roc-start")
                |> cmd_arg("app")
                |> cmd_arg(app_name)
                |> cmd_arg(platform)
                |> cmd_output!
                |> cmd_output_to_str
                |> Ok

cmd_output_to_str = |output_res|
    when output_res is
        { stdout, stderr, status: Ok(_) } ->
            stdout_str =
                stdout
                |> strip_ansi_control
                |> Str.from_utf8
                |> Result.with_default("Could not parse output: Bad UTF-8")
            stderr_str =
                stderr
                |> strip_ansi_control
                |> Str.from_utf8
                |> Result.with_default("Could not parse output: Bad UTF-8")

            "${stdout_str}\n${stderr_str}" |> Str.trim

        { stderr: _, stdout: _, status: Err err} -> output_err_to_str(err)

## Convert a CommandOutputError to a Str
output_err_to_str = |err|
    when err is
        NotFound -> "File not found"
        PermissionDenied -> "Permission denied"
        BrokenPipe -> "Broken pipe"
        AlreadyExists -> "File already exists"
        Interrupted -> "Write interrupted"
        Unsupported -> "Unsupported operation"
        OutOfMemory -> "Out of memory"
        Other(str) -> str

## Strip ANSI control sequences from a list of bytes. (Ensures proper JSON serialization)
strip_ansi_control : List U8 -> List U8
strip_ansi_control = |bytes|
    when List.find_first_index(bytes, |b| b == 27) is
        Ok(escape_index) ->
            { before: lhs, others: remainder } = List.split_at(bytes, escape_index)
            when List.find_first_index(remainder, |b| (b >= 'A' and b <= 'Z') or (b >= 'a' and b <= 'z')) is
                Ok(control_index) ->
                    { before: _, others: rhs } = List.split_at(remainder, (control_index + 1))
                    List.concat(lhs, strip_ansi_control(rhs))

                Err(_) -> bytes

        Err(_) -> bytes

        