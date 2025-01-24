## A prebuilt tool for getting the current UTC time.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [utcNow]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [(utcNow.name, utcNow.handler)]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { get_utc_now!, utc_to_nanos } -> [utc_now]

import InternalTools exposing [Tool, build_tool]
import iso.DateTime

## Expose name, handler and tool for utcNow.
##
## This tool allows the model to get the current UTC time as an ISO 8601 string.
utc_now : { name : Str, handler! : Str => Result Str _, tool : Tool }
utc_now = {
    name: tool.function.name,
    handler!,
    tool,
}

## Tool definition for the utcNow function
tool : Tool
tool = build_tool("utcNow", "Get the current UTC time as an ISO 8601 string", [])

## Handler for the utcNow tool
handler! : Str => Result Str _
handler! = |_args|
    get_utc_now!({})
    |> utc_to_nanos
    |> DateTime.from_nanos_since_epoch
    |> DateTime.to_iso_str
    |> Ok
