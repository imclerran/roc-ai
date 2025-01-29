## A collection of prebuilt tools for interacting with Wikipedia.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [wikipediaSearch, wikipediaParse]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [
##     (wikipediaSearch.name, wikipediaSearch.handler),
##     (wikipediaParse.name, wikipediaParse.handler),
## ]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = Http.send (Chat.buildHttpRequest client messages {}) |> Task.result!
## updatedMessages = updateMessagesFromResponse response messages
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { send_http_req! } -> [wikipedia_search, wikipedia_parse]

import json.Json
import InternalTools exposing [Tool]
import Shared exposing [url_encode]

base_url = "https://en.wikipedia.org/w/api.php"

## Expose name, handler and tool for the wikipediaSarch.
##
## This tool allows the model to search Wikipedia for a given query.
wikipedia_search : { name : Str, handler! : Str => Result Str _, tool : Tool }
wikipedia_search = {
    name: wikipedia_search_tool.function.name,
    handler!: wikipedia_search_handler!,
    tool: wikipedia_search_tool,
}

## Tool definition for the wikepedia search function.
wikipedia_search_tool : Tool
wikipedia_search_tool =
    query_param = {
        name: "search",
        type: "string",
        description: "The search query to use. This can be a single word or a phrase.",
        required: Bool.true,
    }
    limit_param = {
        name: "limit",
        type: "number",
        description: "The number of results to return. This must be a positive integer.",
        required: Bool.true,
    }
    InternalTools.build_tool(
        "wikipedia_search",
        "Search Wikipedia for a given query. This will return a list of articles that match the query.",
        [query_param, limit_param],
    )

## Handler for the wikipedia search tool
wikipedia_search_handler! : Str => Result Str _
wikipedia_search_handler! = |args|
    decoded : Decode.DecodeResult { search : Str, limit : U32 }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ search, limit }) ->
            request = {
                method: Get,
                headers: [],
                url: "${base_url}?action=opensearch&search=${url_encode(search)}&limit=${Num.to_str(limit)}&namespace=0&format=json",
                mime_type: "",
                body: [],
                timeout: NoTimeout,
            }
            when send_http_req!(request) is
                Ok(response) ->
                    response.body
                    |> Str.from_utf8
                    |> Result.with_default("Failed to decode API response")
                    |> Ok

                Err(_) ->
                    "Failed to get response from Wikipedia"
                    |> Ok

## Expose name, handler and tool for the wikipediaParse tool.
##
## This tool allows the model to parse a Wikipedia article.
wikipedia_parse : { name : Str, handler! : Str => Result Str _, tool : Tool }
wikipedia_parse = {
    name: wikipedia_parse_tool.function.name,
    handler!: wikipedia_parse_handler!,
    tool: wikipedia_parse_tool,
}

## Tool definition for the wikipedia parse function
wikipedia_parse_tool : Tool
wikipedia_parse_tool =
    title_param = {
        name: "page",
        type: "string",
        description: "The title of the article to parse. This must be a valid Wikipedia article title, with underscores replacing spaces.",
        required: Bool.true,
    }
    InternalTools.build_tool(
        "wikipedia_parse",
        "Parse a Wikipedia article. This will return the plaintext content of the article.",
        [title_param],
    )

## Handler for the wikipedia parse tool
wikipedia_parse_handler! : Str => Result Str _
wikipedia_parse_handler! = |args|
    decoded : Decode.DecodeResult { page : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ page }) ->
            request = {
                method: Get,
                headers: [],
                url: "${base_url}?action=parse&page=${page}&prop=text&format=json",
                mime_type: "",
                body: [],
                timeout: NoTimeout,
            }
            when send_http_req!(request) is
                Ok(response) ->
                    response.body
                    |> Str.from_utf8
                    |> Result.with_default("Failed to decode API response")
                    |> Ok

                Err(_) ->
                    "Failed to get response from Wikipedia"
                    |> Ok
