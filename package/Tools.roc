module { send_http_req! } -> [Tool, ToolCall, build_tool, handle_tool_calls!, dispatch_tool_calls!]

# import json.Option exposing [Option]
import InternalTools
import Chat
import Client exposing [Client]

## A tool that can be called by the AI model.
## ```
## Tool : {
##     type : Str,
##     function : {
##         name : Str,
##         description : Str,
##         parameters : {
##             type : Str,
##             properties : Dict Str FunctionParameter,
##         },
##         required : List Str,
##     },
## }
## ```
Tool : InternalTools.Tool

## A call from the model to a tool.
## ```
## ToolCall : {
##     id : Str,
##     type : Str,
##     function : {
##         name : Str,
##         arguments : Str,
##     },
## }
## ```
ToolCall : InternalTools.ToolCall

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role : Str,
    content : Str,
    tool_calls : List ToolCall,
    name : Str,
    tool_call_id : Str,
    cached : Bool,
}

## Using the given toolHandlerMap, check the last message for tool calls, call all the tools in the tool call list, send the results back to the model, and handle any additional tool calls that may have been generated. If or when no more tool calls are present, return the updated list of messages.
##
## The Dict maps function tool names strings to roc functions that take their arguments as a JSON string, parse the json, and return the tool's response.
handle_tool_calls! : Client, Dict Str (Str => Result Str _), { max_model_calls ?? U32 } => Result Client _
handle_tool_calls! = |client, tool_handler_map, { max_model_calls ?? Num.max_u32 }|
    when List.last(client.messages) is
        Ok({ role, tool_calls }) if role == "assistant" ->
            if List.is_empty(tool_calls) then #or max_model_calls == 0 then
                Ok(client)
            else
                tc = if max_model_calls > 1 then { tool_choice: Auto } else { tool_choice: None }
                tool_messages = dispatch_tool_calls!(tool_calls, tool_handler_map)?
                client2 = Client.set_messages(client, List.join[client.messages, tool_messages])
                response = send_http_req!(Chat.build_http_request(client2, tc))?
                client3 = Chat.update_message_list(client2, response)?
                handle_tool_calls!(client3, tool_handler_map, { max_model_calls: max_model_calls - 1 })

        _ -> Ok(client)

## Dispatch the tool calls to the appropriate tool handler functions and return the list of tool messages.
##
## The Dict maps function tool names strings to roc functions that take their arguments as a JSON string, parse the json, and return the tool's response.
dispatch_tool_calls! : List ToolCall, Dict Str (Str => Result Str _) => Result (List Message) _
dispatch_tool_calls! = |tool_calls, tool_handler_map|
    dispatch_tool_calls_help!(tool_calls, tool_handler_map, [])

dispatch_tool_calls_help! : List ToolCall, Dict Str (Str => Result Str _), List Message => Result (List Message) _
dispatch_tool_calls_help! = |tool_calls, tool_handler_map, tool_messages|
    when tool_calls is
        [tool_call, .. as remaining_calls] ->
            when Dict.get(tool_handler_map, tool_call.function.name) is
                Ok(handler!) ->
                    tool_message = try call_tool!(tool_call, handler!)
                    updated_tool_messages = List.append(tool_messages, tool_message)
                    dispatch_tool_calls_help!(remaining_calls, tool_handler_map, updated_tool_messages)

                _ ->
                    tool_message = invalid_tool_message(tool_call)
                    updated_tool_messages = List.append(tool_messages, tool_message)
                    dispatch_tool_calls_help!(remaining_calls, tool_handler_map, updated_tool_messages)

        [] -> Ok(tool_messages)

## Call the given tool function with the given arguments and return the tool message.
call_tool! : ToolCall, (Str => Result Str err) => Result Message err
call_tool! = |tool_call, handler!|
    text = try handler!(tool_call.function.arguments)
    (
        {
            role: "tool",
            content: text,
            tool_calls: [],
            tool_call_id: tool_call.id,
            name: tool_call.function.name,
            cached: Bool.false,
        }
    )
    |> Ok

invalid_tool_message : ToolCall -> Message
invalid_tool_message = |tool_call| {
    role: "tool",
    content: "Error: the requested tool could not be found on the host machine.",
    tool_calls: [],
    tool_call_id: tool_call.id,
    name: tool_call.function.name,
    cached: Bool.false,
}

## Build a tool object with the given name, description, and parameters.
## ```
## buildTool = \name, description, parameters -> ...
## ```
## Parameters:
## - `name : Str` : The name of the tool.
## - `description : Str` : The description of the tool.
## - `parameters : List { ... }` : The parameters for the tool.
##     - `name : Str` : The name of the parameter.
##     - `type : Str` : The type of the parameter.
##     - `description : Str` : The description of the parameter.
##     - `required : Bool` : Whether the parameter is required.
##
## Returns:
## - `Tool` : The tool object.
build_tool : Str, Str, List { name : Str, type : Str, description : Str, required : Bool } -> Tool
build_tool = InternalTools.build_tool
