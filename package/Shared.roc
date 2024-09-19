module [ApiError, TimeoutConfig, ErrorResponse, RequestObject, ResponseFormat, dropLeadingGarbage, decodeErrorResponse, optionToStr, optionToList]

import json.Json
import json.Option exposing [Option]

## Redefinition of TimeoutConfig from the basic-cli Http module
TimeoutConfig : [TimeoutMilliseconds U64, NoTimeout]

## The request object to be sent with basic-cli's Http.send
RequestObject : {
    method : [Post],
    headers : List { key : Str, value : Str },
    mimeType : Str,
    url : Str,
    body : List U8,
    timeout : TimeoutConfig,
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

## Drop leading garbage characters from the response body
dropLeadingGarbage : List U8 -> List U8
dropLeadingGarbage = \bytes ->
    when List.findFirstIndex bytes \elem -> elem > ' ' is
        Ok idx -> List.dropFirst bytes idx
        Err _ -> bytes

## Decode the JSON response body of an API error message
decodeErrorResponse : List U8 -> Result ErrorResponse _
decodeErrorResponse = \bodyBytes ->
    cleanedBody = dropLeadingGarbage bodyBytes
    decoder = Json.utf8With { fieldNameMapping: SnakeCase }
    decoded : Decode.DecodeResult ErrorResponse
    decoded = Decode.fromBytesPartial cleanedBody decoder
    decoded.result

optionToStr : Option Str -> Str
optionToStr = \opt ->
    when Option.get opt is
        Some str -> str
        None -> ""

optionToList : Option (List a) -> List a
optionToList = \opt ->
    when Option.get opt is
        Some list -> list
        None -> []
