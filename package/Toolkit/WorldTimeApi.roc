## A prebuilt tool for interacting with the WorldTimeApi.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [current_time.tool]
## # Tool handler map is passed to Tools.handle_tool_calls!
## tool_handler_map = Dict.from_list([(currentTime.name, currentTime.handler!)])
## client = Client.new { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.add_user_message(client, newMessage, {})
## response = Http.send!(Chat.build_http_request(client, {}))?
## with_tool_results = 
##      Chat.update_messages(response, messages)?
##     |> Tools.handle_tool_calls!(client toolHandlerMap, { max_model_calls: 5 })
## ```
module { send_http_req! } -> [current_time]

import InternalTools exposing [Tool]
import json.Json

## Expose name, handler and tool for the currentTime.
##
## This tool allows the model to get the current time data for a given timezone.
current_time : { name : Str, handler! : Str => Result Str _, tool : Tool }
current_time = {
    name: tool.function.name,
    handler!,
    tool,
}

## Tool definition for the currentTime function
tool : Tool
tool =
    tz_param = {
        name: "tz",
        type: "string",
        description: "The timezone to get the current time for. Must be a valid canonical timezone name. Eg: 'America/Chicago'",
        required: Bool.true,
    }
    InternalTools.build_tool(
        "current_time",
        """
        Get the current time data for a given timezone. This includes: utc_offset, timezone, day_of_week, day_of_year,
        datetime, utc_datetime, unixtime, raw_offset, week_number, dst, abbreviation, dst_offset, dst_from, dst_until.
        """,
        [tz_param],
    )

## Handler for the currentTime tool
handler! : Str => Result Str _
handler! = |args|
    decoded : Decode.DecodeResult { tz : Str }
    decoded =
        args
        |> Str.to_utf8
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ tz }) ->
            request = {
                method: Get,
                headers: [],
                url: "http://worldtimeapi.org/api/timezone/${tz}",
                mime_type: "application/json",
                body: [],
                timeout: NoTimeout,
            }
            when send_http_req!(request) is
                Ok(response) ->
                    response.body
                    |> remove_ip
                    |> Str.from_utf8
                    |> Result.with_default("Failed to decode API response")
                    |> Ok

                Err(_) ->
                    "Failed to get response from worldtimeapi.org"
                    |> Ok

## WorldTimeApi response, with client ip removed
ApiResponse : {
    utc_offset : Str,
    timezone : Str,
    day_of_week : U32,
    day_of_year : U32,
    datetime : Str,
    utc_datetime : Str,
    unixtime : U32,
    raw_offset : I32,
    week_number : U32,
    dst : Bool,
    abbreviation : Str,
    dst_offset : I32,
    dst_from : Str,
    dst_until : Str,
}

## Remove the client ip from the response to ensure no personal data sent to the model
remove_ip : List U8 -> List U8
remove_ip = |bytes|
    decoded : Decode.DecodeResult ApiResponse
    decoded = Decode.from_bytes_partial(bytes, Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Ok(response) -> response |> Encode.to_bytes(Json.utf8_with({ field_name_mapping: SnakeCase }))
        Err(_) -> "Failed to decode response" |> Str.to_utf8
