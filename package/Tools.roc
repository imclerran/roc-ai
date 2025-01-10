module { send_http_req } -> [Tool, ToolCall, build_tool, handle_tool_calls, dispatch_tool_calls]

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
handle_tool_calls : List Message, Client, Dict Str (Str -> Task Str _), { max_model_calls ?? U32 } -> Task (List Message) _
handle_tool_calls = \messages, client, tool_handler_map, { max_model_calls ?? Num.max_u32 } ->
    when List.last(messages) is
        Ok({ role, tool_calls }) if role == "assistant" ->
            if List.is_empty(tool_calls) || max_model_calls == 0 then
                Task.ok(messages)
            else
                tc = if max_model_calls > 1 then { tool_choice: Auto } else { tool_choice: None }
                tool_messages = dispatch_tool_calls!(tool_calls, tool_handler_map)
                messages_with_tools = List.join([messages, tool_messages])
                response = send_http_req(Chat.build_http_request(client, messages_with_tools, tc)) |> Task.result!
                messages_with_response = Chat.update_message_list(response, messages_with_tools)
                handle_tool_calls(messages_with_response, client, tool_handler_map, { max_model_calls: max_model_calls - 1 })

        _ -> Task.ok(messages)

## Dispatch the tool calls to the appropriate tool handler functions and return the list of tool messages.
##
## The Dict maps function tool names strings to roc functions that take their arguments as a JSON string, parse the json, and return the tool's response.
dispatch_tool_calls : List ToolCall, Dict Str (Str -> Task Str _) -> Task (List Message) _
dispatch_tool_calls = \tool_call_list, tool_handler_map ->
    Task.loop({ tool_calls: tool_call_list, tool_messages: [] }, \{ tool_calls, tool_messages } ->
        when List.first(tool_calls) is
            Ok(tool_call) ->
                when tool_handler_map |> Dict.get(tool_call.function.name) is
                    Ok(handler) ->
                        tool_message = call_tool!(tool_call, handler)
                        updated_tool_messages = List.append(tool_messages, tool_message)
                        Task.ok(Step({ tool_calls: List.drop_first(tool_calls, 1), tool_messages: updated_tool_messages }))

                    _ ->
                        Task.ok(Step({ tool_calls: List.drop_first(tool_calls, 1), tool_messages }))

            Err(ListWasEmpty) -> Task.ok(Done(tool_messages)))

## Call the given tool function with the given arguments and return the tool message.
call_tool : ToolCall, (Str -> Task Str err) -> Task Message err
call_tool = \tool_call, handler ->
    Task.map(handler(tool_call.function.arguments), \text -> {
        role: "tool",
        content: text,
        tool_calls: [],
        tool_call_id: tool_call.id,
        name: tool_call.function.name,
        cached: Bool.false,
    })

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
