module { sendHttpReq! } -> [
    Tool,
    ToolCall,
    buildTool,
    handleToolCalls!,
    dispatchToolCalls!,
]

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
    toolCalls : List ToolCall,
    name : Str,
    toolCallId : Str,
    cached : Bool,
}

## Using the given toolHandlerMap, check the last message for tool calls, call all the tools in the tool call list, send the results back to the model, and handle any additional tool calls that may have been generated. If or when no more tool calls are present, return the updated list of messages.
##
## The Dict maps function tool names strings to roc functions that take their arguments as a JSON string, parse the json, and return the tool's response.
handleToolCalls! : List Message, Client, Dict Str (Str => Result Str _), { maxModelCalls ? U32 } => Result (List Message) _
handleToolCalls! = \messages, client, toolHandlerMap, { maxModelCalls ? Num.maxU32 } ->
    when List.last messages is
        Ok { role, toolCalls } if role == "assistant" ->
            if List.isEmpty toolCalls || maxModelCalls == 0 then
                Ok messages
            else
                tc = if maxModelCalls > 1 then { toolChoice: Auto } else { toolChoice: None }
                toolMessages = try dispatchToolCalls! toolCalls toolHandlerMap
                messagesWithTools = List.join [messages, toolMessages]
                response = try sendHttpReq! (Chat.buildHttpRequest client messagesWithTools tc)
                messagesWithResponse = Chat.updateMessageList response messagesWithTools
                handleToolCalls! messagesWithResponse client toolHandlerMap { maxModelCalls: maxModelCalls - 1 }

        _ -> Ok messages

## Dispatch the tool calls to the appropriate tool handler functions and return the list of tool messages.
##
## The Dict maps function tool names strings to roc functions that take their arguments as a JSON string, parse the json, and return the tool's response.
dispatchToolCalls! : List ToolCall, Dict Str (Str => Result Str _) => Result (List Message) _
dispatchToolCalls! = \toolCalls, toolHandlerMap -> dispatchToolCallsLoop! toolCalls toolHandlerMap []

dispatchToolCallsLoop! : List ToolCall, Dict Str (Str => Result Str _), List Message => Result (List Message) _
dispatchToolCallsLoop! = \toolCalls, toolHandlerMap, toolMessages ->
    when toolCalls is
        [toolCall, .. as rest] ->
            when toolHandlerMap |> Dict.get toolCall.function.name is
                Ok handler! ->
                    toolMessage = try callTool! toolCall handler!
                    updatedToolMessages = List.append toolMessages toolMessage
                    dispatchToolCallsLoop! rest toolHandlerMap updatedToolMessages

                _ ->
                    toolMessage = toolDoesNotExistMessage toolCall
                    updatedToolMessages = List.append toolMessages toolMessage
                    dispatchToolCallsLoop! rest toolHandlerMap updatedToolMessages

        [] -> Ok toolMessages

## Call the given tool function with the given arguments and return the tool message.
callTool! : ToolCall, (Str => Result Str err) => Result Message err
callTool! = \toolCall, handler! ->
    Result.map (handler! toolCall.function.arguments) \text -> {
        role: "tool",
        content: text,
        toolCalls: [],
        toolCallId: toolCall.id,
        name: toolCall.function.name,
        cached: Bool.false,
    }

toolDoesNotExistMessage : ToolCall -> Message
toolDoesNotExistMessage = \toolCall -> 
    toolName = toolCall.function.name
    {
        role: "tool",
        content: "Error: the tool $(toolName) was not found on the client.",
        toolCalls: [],
        toolCallId: toolCall.id,
        name: toolCall.function.name,
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
buildTool : Str, Str, List { name : Str, type : Str, description : Str, required : Bool } -> Tool
buildTool = InternalTools.buildTool
