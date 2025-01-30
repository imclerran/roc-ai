## A prebuilt tool for interacting with Wolfram Alpha.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [wolfram_short_answer.tool]
## # Tool handler map is passed to Tools.handle_tool_calls!
## tool_handler_map = Dict.from_list([
##     (wolfram_short_answer.name, wolfram_short_answer.handler!),
## ])
## client = Client.new { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.add_user_message(client, newMessage, {})
## response = Http.send!(Chat.build_http_request(client, {}))?
## with_tool_results = 
##      Chat.update_messages(response, messages)?
##     |> Tools.handle_tool_calls!(client toolHandlerMap, { max_model_calls: 5 })
## ```
module { send_http_req!, get_env_var! } -> [wolfram_short_answer]

import json.Json
import InternalTools exposing [Tool]
import Shared exposing [url_encode]

## Expose name, handler and tool for shortAnswer.
##
## This tool allows the model to ask Wolfram Alpha a question and get a short answer.
wolfram_short_answer : { name : Str, handler! : Str => Result Str _, tool : Tool }
wolfram_short_answer = {
    name: short_answer_tool.function.name,
    handler!: short_answer_handler!,
    tool: short_answer_tool,
}

## Tool definition for the shortAnswer function
short_answer_tool : Tool
short_answer_tool =
    input_param = {
        name: "input",
        type: "string",
        description: "The question to ask Wolfram Alpha.",
        required: Bool.true,
    }
    InternalTools.build_tool(
        "wolfram_short_answer",
        """
        Ask Wolfram Alpha a question and get a short answer. 
        Wolfram can answer questions in many categories, including but not limited to:
        Mathematical computations, unit conversions, fact-based queries, scientific 
        questions, weather and location based data, date and time queries, financial 
        and economic data, historical events, and general knowledge questions.
        """,
        [input_param],
    )

## Handler for the shortAnswer tool
short_answer_handler! : Str => Result Str _
short_answer_handler! = |args|
    decoded : Decode.DecodeResult { input : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ input }) ->
            app_id = try get_env_var!("WOLFRAMALPHA_APP_ID")
            request = {
                method: Get,
                headers: [],
                url: "http://api.wolframalpha.com/v1/result?i=${url_encode(input)}&appid=${app_id}",
                mime_type: "application/json",
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
                    "Failed to get response from Wolfram Alpha"
                    |> Ok
