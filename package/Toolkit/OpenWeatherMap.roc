## A collection of prebuilt tools for interacting with the OpenWeatherMap API.
## ```
## # USAGE:
## # Tool list to initialize the client
## tools = [geocoding, currentWeather]
## # Tool handler map is passed to Tools.handleToolCalls!
## toolHandlerMap = Dict.fromList [
##     (geocoding.name, geocoding.handler),
##     (currentWeather.name, currentWeather.handler),
## ]
## client = Client.init { apiKey, model: "tool-capable/model", tools }
## #...
## messages = Chat.appendUserMessage previousMessages newMessage
## response = try Http.send (Chat.buildHttpRequest client messages {})
## updatedMessages = updateMessagesFromResponse response messages
##     |> Tools.handleToolCalls! client toolHandlerMap
## ```
module { send_http_req!, get_env_var! } -> [geocoding, current_weather]

import json.Json
import InternalTools exposing [Tool]

## Expose name, handler and tool for geocoding.
##
## This tool will allow the model to get the correct latitude and longitude for a location, for use with the currentWeather tool.
geocoding : { name : Str, handler! : Str => Result Str _, tool : Tool }
geocoding = {
    name: geocoding_tool.function.name,
    handler!: geocoding_handler!,
    tool: geocoding_tool,
}

## Tool definition for the geocoding function
geocoding_tool : Tool
geocoding_tool =
    query_param = {
        name: "q",
        type: "string",
        description:
        "City name, state code (only for the US) and country code divided by comma. Please use ISO 3166 country codes. city name, state code, and country code are all required for US cities, and city name and country code are required for all other cities. Do not leave out the 2 letter state code for US cities. For example, to get the location of New York City, you would use 'Chicago,IL,US'. For London, you would use 'London,GB'. You should also make sure that any spaces in city names are URL encoded. Last, make sure there are no spaces between the city name, state code, and country code.",
        required: Bool.true,
    }
    InternalTools.build_tool("geocoding", "Geocode a location using the openweathermap.org API", [query_param])

## Handler for the geocoding tool
geocoding_handler! : Str => Result Str _
geocoding_handler! = |args|
    decoded : Decode.DecodeResult { q : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ q }) ->
            api_key = get_env_var!("OPENWEATHERMAP_API_KEY")?
            request = {
                method: GET,
                headers: [{ name: "Content-Type", value: "application/json" }],
                uri: "http://api.openweathermap.org/geo/1.0/direct?q=${q}&appid=${api_key}",
                body: [],
                timeout_ms: NoTimeout,
            }
            when send_http_req!(request) is
                Ok(response) ->
                    response.body
                    |> Str.from_utf8
                    |> Result.with_default("Failed to decode API response")
                    |> Ok

                Err(_) ->
                    "Failed to get response from openweathermap.org"
                    |> Ok

## Expose name, handler and tool for currentWeather
##
## This tool will allow the model to get the current weather for a location.
current_weather : { name : Str, handler! : Str => Result Str _, tool : Tool }
current_weather = {
    name: current_weather_tool.function.name,
    handler!: current_weather_handler!,
    tool: current_weather_tool,
}

## Tool definition for the currentWeather function
current_weather_tool : Tool
current_weather_tool =
    lat_param = {
        name: "lat",
        type: "number",
        description: "The latitude of the location to get the current weather for. Decimal from -90 to 90.",
        required: Bool.true,
    }
    lon_param = {
        name: "lon",
        type: "number",
        description: "The longitude of the location to get the current weather for. Decimal from -180 to 180.",
        required: Bool.true,
    }
    units_param = {
        name: "units",
        type: "string",
        description:
        "The units to return the weather in. Can be 'standard', 'metric', or 'imperial'. Standard is Kelvin, metric is Celsius, and imperial is Fahrenheit.",
        required: Bool.true,
    }
    InternalTools.build_tool("current_weather", "Get the current weather for a location using the openweathermap.org API", [lat_param, lon_param, units_param])

## Handler for the currentWeather tool
current_weather_handler! : Str => Result Str _
current_weather_handler! = |args|
    decoded : Decode.DecodeResult { lat : F32, lon : F32, units : Str }
    decoded = 
        args 
        |> Str.to_utf8 
        |> Decode.from_bytes_partial(Json.utf8_with({ field_name_mapping: SnakeCase }))
    when decoded.result is
        Err(TooShort) ->
            Ok("Failed to decode args")

        Ok({ lat, lon, units }) ->
            exclude = "minutely,hourly,daily,alerts"
            api_key = get_env_var!("OPENWEATHERMAP_API_KEY")?
            request = {
                method: GET,
                headers: [{ name: "Content-Type", value: "application/json" }],
                uri: "https://api.openweathermap.org/data/3.0/onecall?lat=${Num.to_str(lat)}&lon=${Num.to_str(lon)}&exclude=${exclude}&units=${units}&appid=${api_key}",
                body: [],
                timeout_ms: NoTimeout,
            }
            when send_http_req!(request) is
                Ok(response) ->
                    response.body
                    |> Str.from_utf8
                    |> Result.with_default("Failed to decode API response")
                    |> Ok

                Err(_) ->
                    "Failed to get response from openweathermap.org"
                    |> Ok

