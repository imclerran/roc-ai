module [
    ApiError,
    TimeoutConfig,
    ErrorResponse,
    RequestObject,
    ResponseFormat,
    HttpResponse,
    Message,
    ApiTarget,
    drop_leading_garbage,
    decode_error_response,
    option_to_str,
    option_to_list,
    url_encode,
    str_to_option,
    list_to_option,
]

import json.Json
import json.Option exposing [Option]

## Redefinition of TimeoutConfig from the basic-cli Http module
TimeoutConfig : [TimeoutMilliseconds U64, NoTimeout]

## The request object to be sent with basic-cli's Http.send
RequestObject : {
    method : [POST, GET],
    headers : List { name : Str, value : Str },
    uri : Str,
    body : List U8,
    timeout_ms : TimeoutConfig,
}

## The structure of the JSON error response from the OpenAI API
ErrorResponse : {
    error : ApiError,
}

## The API error status code and description
ApiError : {
    code : U16,
    message : Str,
}

## Tells the LLM how to respond to the user. Should be either "text" or "json_object"
ResponseFormat : {
    type : Str,
}

## Represents an HTTP response.
HttpResponse : {
    status : U16,
    headers : List { name : Str, value : Str },
    body : List U8,
}

Message : {
    role : Str,
    content : Str,
    reasoning_content : Str,
    tool_calls : List ToolCall,
    name : Str,
    tool_call_id : Str,
    cached : Bool,
}

## A call from the model to a tool.
ToolCall : {
    id : Str,
    type : Str,
    function : {
        name : Str,
        arguments : Str,
    },
}

ApiTarget : [OpenRouter, OpenAI, Anthropic, OpenAICompliant { url : Str }]

## Drop leading garbage characters from the response body
drop_leading_garbage : List U8 -> List U8
drop_leading_garbage = |bytes|
    when List.find_first_index(bytes, |elem| elem > ' ') is
        Ok(idx) -> List.drop_first(bytes, idx)
        Err(_) -> bytes

## Decode the JSON response body of an API error message
decode_error_response : List U8 -> Result ErrorResponse [TooShort]
decode_error_response = |body_bytes|
    cleaned_body = drop_leading_garbage(body_bytes)
    decoder = Json.utf8_with({ field_name_mapping: SnakeCase })
    decoded : Decode.DecodeResult ErrorResponse
    decoded = Decode.from_bytes_partial(cleaned_body, decoder)
    decoded.result

## Convert an Option to a string
option_to_str : Option Str -> Str
option_to_str = |opt|
    when Option.get(opt) is
        Some(str) -> str
        None -> ""

## Convert a string to an Option
str_to_option : Str -> Option Str
str_to_option = |str|
    when str is
        "" -> Option.none({})
        _ -> Option.some(str)

## Convert an Option to a List
option_to_list : Option (List a) -> List a
option_to_list = |opt|
    when Option.get(opt) is
        Some(list) -> list
        None -> []

## Convert a List to an Option
list_to_option : List a -> Option (List a)
list_to_option = |list|
    when list is
        [] -> Option.none({})
        _ -> Option.some(list)

## URL-encode a string
url_encode : Str -> Str
url_encode = |str|
    str
    |> Str.to_utf8
    |> List.map(
        |char|
            Dict.get(url_encode_dict, char)
            |> Result.with_default(
                ([char] |> Str.from_utf8 |> Result.with_default("")),
            ),
    )
    |> Str.join_with("")

## Dictionary of characters to URL-encoded strings
url_encode_dict : Dict U8 Str
url_encode_dict =
    Dict.empty({})
    |> Dict.insert(' ', "%20")
    |> Dict.insert('!', "%21")
    |> Dict.insert('"', "%22")
    |> Dict.insert('#', "%23")
    |> Dict.insert('$', "%24")
    |> Dict.insert('%', "%25")
    |> Dict.insert('&', "%26")
    |> Dict.insert('\'', "%27")
    |> Dict.insert('(', "%28")
    |> Dict.insert(')', "%29")
    |> Dict.insert('*', "%2A")
    |> Dict.insert('+', "%2B")
    |> Dict.insert(',', "%2C")
    |> Dict.insert('-', "%2D")
    |> Dict.insert('.', "%2E")
    |> Dict.insert('/', "%2F")
    |> Dict.insert(':', "%3A")
    |> Dict.insert(';', "%3B")
    |> Dict.insert('<', "%3C")
    |> Dict.insert('=', "%3D")
    |> Dict.insert('>', "%3E")
    |> Dict.insert('?', "%3F")
    |> Dict.insert('@', "%40")
    |> Dict.insert('[', "%5B")
    |> Dict.insert('\\', "%5C")
    |> Dict.insert(']', "%5D")
    |> Dict.insert('^', "%5E")
    |> Dict.insert('_', "%5F")
    |> Dict.insert('`', "%60")
    |> Dict.insert('{', "%7B")
    |> Dict.insert('|', "%7C")
    |> Dict.insert('}', "%7D")
    |> Dict.insert('~', "%7E")
