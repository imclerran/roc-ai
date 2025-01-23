## A prebuilt tool for interacting with the serper.dev google search API.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [serper]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [(serper.name, serper.handler)]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { send_http_req!, get_env_var! } -> [serper]

import InternalTools exposing [Tool, build_tool]

## Expose name, handler and tool for serper.
##
## This tool allows the model to search google using the serper.dev API.
serper : { name : Str, handler! : Str => Result Str *, tool : Tool }
serper = {
    name: tool.function.name,
    handler!,
    tool,
}

## Tool definition for the serper function
tool : Tool
tool =
    query_param = {
        name: "q",
        type: "string",
        description: "The search query to send to the serper.dev API",
        required: Bool.true,
    }
    build_tool("serper", "Access to the serper.dev google search API", [query_param])

## Handler for the serper tool
handler! : Str => Result Str _
handler! = |args|
    api_key = try get_env_var!("SERPER_API_KEY")
    request = {
        method: POST,
        headers: [
            { name: "X-API-KEY", value: api_key },
            { name: "Content-Type", value: "application/json" },
        ],
        uri: "https://google.serper.dev/search",
        body: args |> Str.to_utf8,
        timeout_ms: NoTimeout,
    }
    when send_http_req!(request) is
        Ok(response) ->
            response.body
            |> Str.from_utf8
            |> Result.with_default("Failed to decode API response")
            |> Ok

        Err(_) ->
            "Failed to get response from serper.dev"
            |> Ok
