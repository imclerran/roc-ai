## A prebuilt tool for getting the current UTC time.
## ```
# # USAGE:
## # Tool list to initialize the client
## tools = [utc_now.tool]
## # Tool handler map is passed to Tools.handle_tool_calls!
## tool_handler_map = Dict.from_list([(utc_now.name, utc_now.handler)])
## client = Client.new { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.add_user_message(client, newMessage, {})
## response = Http.send!(Chat.build_http_request(client, {}))?
## with_tool_results = 
##      Chat.update_messages(response, messages)?
##     |> Tools.handle_tool_calls!(client toolHandlerMap, { max_model_calls: 5 })
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
tool = build_tool("utc_now", "Get the current UTC time as an ISO 8601 string", [])

## Handler for the utcNow tool
handler! : Str => Result Str _
handler! = |_args|
    get_utc_now!({})
    |> utc_to_nanos
    |> DateTime.from_nanos_since_epoch
    |> DateTime.to_iso_str
    |> Ok
