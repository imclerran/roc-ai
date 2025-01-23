## The Chat module contains the functions and types needed to use the ChatML formatted chat completion API. It includes the Message type, ChatRequestBody and ChatResponseBody types, and various functions for creating and handling API requests.
module [
    ChatRequestBody,
    ChatResponseBody,
    Message,
    Client,
    append_assistant_message,
    append_system_message,
    append_user_message,
    build_http_request,
    decode_error_response,
    decode_response,
    update_message_list,
    decode_top_message_choice,
    encode_request_body,
    new_client,
]

import json.Json
import json.Option exposing [Option]

import Client
import InternalTools exposing [ToolCall, ToolChoice]
import Shared exposing [
    RequestObject,
    ApiError,
    HttpResponse,
    drop_leading_garbage,
    option_to_str,
    option_to_list,
    str_to_option,
    list_to_option,
]

Client : Client.Client

## The OpenAI ChatML standard message used to query the AI model.
Message : {
    role : Str,
    content : Str,
    tool_calls : List ToolCall,
    name : Str,
    tool_call_id : Str,
    cached : Bool,
}

## Internal ChatML message to decode messages from JSON. Allows optional fields.
DecodeMessage : {
    role : Str,
    content : Option Str,
    tool_calls : Option (List ToolCall),
    name : Option Str,
    tool_call_id : Option Str,
}

## The structure of cached messages to be encoded to JSON for the API.
EncodeCacheMessage : {
    role : Str,
    content : List CacheContent,
    tool_calls : Option (List ToolCall),
    name : Option Str,
    tool_call_id : Option Str,
}

## The structure of non-cached messages to be encoded to JSON for the API.
EncodeBasicMessage : {
    role : Str,
    content : Str,
    tool_calls : Option (List ToolCall),
    name : Option Str,
    tool_call_id : Option Str,
}

## The message content of a cacheable message.
CacheContent : {
    type : Str,
    text : Str,
    cache_control : Option { type : Str },
}

## The structure of the request body to be sent in the Http request.
ChatRequestBody : {
    model : Str,
    messages : List Message,
    temperature : F32,
    top_a : F32,
    top_p : F32,
    top_k : U64,
    frequency_penalty : F32,
    presence_penalty : F32,
    repetition_penalty : F32,
    min_p : F32,
    seed : Option U64,
    max_tokens : Option U64,
    provider : {
        order : Option (List Str),
    },
    models : Option (List Str),
    route : Option Str,
    # tools: Option (List Tools.Tool),
    # toolChoice: Option Tools.ToolChoice,
}

## The structure of the JSON response body received from the OpenRouter API.
ChatResponseBody : {
    id : Str,
    model : Str,
    object : Str,
    created : U64,
    choices : List {
        index : U8,
        message : Message,
        finish_reason : Str,
    },
    usage : {
        prompt_tokens : U64,
        completion_tokens : U64,
        total_tokens : U64,
    },
}

## Internal version of the chat response body to decode JSON responses.
DecodeChatResponseBody : {
    id : Str,
    model : Str,
    object : Str,
    created : U64,
    choices : List {
        index : U8,
        message : DecodeMessage,
        finish_reason : Option Str,
    },
    usage : {
        prompt_tokens : U64,
        completion_tokens : U64,
        total_tokens : U64,
    },
}

## Initialize the OpenRouter API client with the required API key. All parameters besides apiKey are completely optional, and may be set during initialization, assigned later, or left as their defaults.
## ```
## client = Chat.new_client { apiKey: "your_openrouter_api_key" }
## ```
## Same as `Client.new`.
new_client = Client.new

## Create a request object to be sent with basic-cli's Http.send using ChatML messages.
build_http_request : Client, List Message, { tool_choice ?? ToolChoice } -> RequestObject
build_http_request = |client, messages, { tool_choice ?? Auto }|
    body = build_request_body(client)
    tools =
        when Option.get(client.tools) is
            Some(tool_list) -> tool_list
            None -> []
    {
        method: POST,
        headers: [
            { name: "Authorization", value: "Bearer ${client.api_key}" },
            { name: "anthropic-version", value: "2023-06-01" },
            { name: "x-api-key", value: client.api_key },
            { name: "content-type", value: "application/json" },
        ],
        uri: client.url,
        body: encode_request_body(body)
        |> inject_messages(messages)
        |> InternalTools.inject_tools(tools)
        |> |bytes|
            if List.is_empty tools then
                bytes
            else
                InternalTools.inject_tool_choice(bytes, tool_choice),
        timeout_ms: client.request_timeout,
    }

## Build the request body to be sent in the Http request using ChatML messages.
build_request_body : Client -> ChatRequestBody
build_request_body = |client| {
    messages: [],
    model: client.model,
    temperature: client.temperature,
    top_a: client.top_a,
    top_p: client.top_p,
    top_k: client.top_k,
    frequency_penalty: client.frequency_penalty,
    presence_penalty: client.presence_penalty,
    repetition_penalty: client.repetition_penalty,
    min_p: client.min_p,
    seed: client.seed,
    max_tokens: client.max_tokens,
    provider: { order: client.provider_order },
    models: client.models,
    route: client.route,
}

## Decode the JSON response body to a ChatML style request.
decode_response : List U8 -> Result ChatResponseBody _
decode_response = |body_bytes|
    cleaned_body = drop_leading_garbage(body_bytes)
    decoder = Json.utf8_with({ field_name_mapping: SnakeCase })
    decoded : Decode.DecodeResult DecodeChatResponseBody
    decoded = Decode.from_bytes_partial(cleaned_body, decoder)
    decoded.result
    |> Result.map_ok(
        |internal_response| {
            id: internal_response.id,
            model: internal_response.model,
            object: internal_response.object,
            created: internal_response.created,
            choices: internal_response.choices
            |> List.map(
                |{ index, message: decode_message, finish_reason: internal_finish_reason }| {
                    index,
                    message: convert_decode_message(decode_message),
                    finish_reason: option_to_str(internal_finish_reason),
                },
            ),
            usage: internal_response.usage,
        },
    )

## Convert an DecodeMessage to a Message.
convert_decode_message : DecodeMessage -> Message
convert_decode_message = |decode_message| {
    role: decode_message.role,
    content: option_to_str(decode_message.content),
    tool_calls: option_to_list(decode_message.tool_calls),
    tool_call_id: option_to_str(decode_message.tool_call_id),
    name: option_to_str(decode_message.name),
    cached: Bool.false,
}

## Build a CacheContent object for a message.
build_message_content : Str, Bool -> CacheContent
build_message_content = |text, cached| {
    type: "text",
    text,
    cache_control: if cached then Option.some({ type: "ephemeral" }) else Option.none({}),
}

## Decode the JSON response body to the first message in the list of choices.
decode_top_message_choice : List U8 -> Result Message [ApiError ApiError, DecodingError, NoChoices, BadJson Str]
decode_top_message_choice = |response_body_bytes|
    when decode_response(response_body_bytes) is
        Ok(body) ->
            when List.get(body.choices, 0) is
                Ok(choice) -> Ok(choice.message)
                Err(_) -> Err(NoChoices)

        Err(_) ->
            when decode_error_response(response_body_bytes) is
                Ok(err) -> Err(ApiError(err.error))
                Err(_) ->
                    when response_body_bytes |> Str.from_utf8 is
                        Ok(str) -> Err(BadJson(str))
                        Err(_) -> Err(DecodingError)

## Decode the JSON response body of an API error message.
decode_error_response = Shared.decode_error_response

## Decode the response from the OpenRouter API and append the first message choice to the list of messages. Any errors encountered will be appended as system messages.
update_message_list : HttpResponse, List Message -> List Message
update_message_list = |response, messages|
    if response.status >= 200 and response.status <= 299 then
        when decode_top_message_choice(response.body) is
            Ok(message) -> List.append(messages, message)
            Err(ApiError(err)) -> append_system_message(messages, "API error: ${Inspect.to_str(err)}", {}) # err.message
            Err(NoChoices) -> append_system_message(messages, "No choices in API response", {})
            Err(BadJson(str)) -> append_system_message(messages, "Could not decode JSON response:\n${str}", {})
            Err(DecodingError) -> append_system_message(messages, "Error decoding API response", {})
        else

    message =
        Str.from_utf8(response.body)
        |> Result.with_default("")
        |> Str.with_prefix("Http error: ${Num.to_str(response.status)}\n")
    append_system_message(messages, message, {})

# when response_res is
#     Ok(response) ->
#         when decode_top_message_choice(response.body) is
#             Ok(message) -> List.append(messages, message)
#             Err(ApiError(err)) -> append_system_message(messages, "API error: ${Inspect.to_str(err)}", {}) # err.message
#             Err(NoChoices) -> append_system_message(messages, "No choices in API response", {})
#             Err(BadJson(str)) -> append_system_message(messages, "Could not decode JSON response:\n${str}", {})
#             Err(DecodingError) -> append_system_message(messages, "Error decoding API response", {})

#     Err(HttpErr(BadStatus({ code }))) ->
#         append_system_message(messages, "Http error: ${Num.to_str(code)}", {})

#     Err(HttpErr(_)) ->
#         append_system_message(messages, "Http error", {})

## Encode the request body to be sent in the Http request.
encode_request_body : ChatRequestBody -> List U8
encode_request_body = |body|
    Encode.to_bytes(
        body,
        Json.utf8_with(
            {
                field_name_mapping: SnakeCase,
                empty_encode_as_null: Json.encode_as_null_option({ record: Bool.false }),
            },
        ),
    )

## Inject the messages list into the request body, by encoding the message to the correct format based on the cached flag.
inject_messages : List U8, List Message -> List U8
inject_messages = |body_bytes, messages|
    inject_at = List.walk_with_index_until(
        body_bytes,
        0,
        |_, _, i|
            when List.drop_first(body_bytes, i) is
                ['m', 'e', 's', 's', 'a', 'g', 'e', 's', '"', ':', '[', ..] -> Break((i + 11))
                ['m', 'e', 's', 's', 'a', 'g', 'e', 's', '"', ':', ' ', '[', ..] -> Break((i + 12))
                _ -> Continue(0),
    )

    if inject_at == 0 then
        body_bytes
    else
        { before, others } = List.split_at(body_bytes, inject_at)
        message_bytes =
            messages
            |> List.map(
                |message|
                    if message.cached and message.tool_call_id == "" then
                        bytes =
                            message_to_cache_message(message)
                            |> Encode.to_bytes(Json.utf8_with({ field_name_mapping: SnakeCase, empty_encode_as_null: Json.encode_as_null_option({ record: Bool.false }) }))
                            |> List.append(',')
                        bytes
                        |> List.drop_at(List.len(bytes) - 3) # drop the last comma before the closing bracket
                    else
                        bytes =
                            message_to_basic_message(message)
                            |> Encode.to_bytes(Json.utf8_with({ field_name_mapping: SnakeCase, empty_encode_as_null: Json.encode_as_null_option({ record: Bool.false }) }))
                            |> List.append(',')
                        bytes
                        |> List.drop_at(List.len(bytes) - 3), # drop the last comma before the closing bracket
            )
            |> List.join
            |> List.drop_last(1)
        List.join([before, message_bytes, others])

## Convert a Message to an EncodeCacheMessage.
message_to_cache_message : Message -> EncodeCacheMessage
message_to_cache_message = |message| {
    role: message.role,
    content: [build_message_content(message.content, message.cached)],
    tool_calls: list_to_option(message.tool_calls),
    tool_call_id: str_to_option(message.tool_call_id),
    name: str_to_option(message.name),
}

## Convert a Message to an EncodeBasicMessage.
message_to_basic_message : Message -> EncodeBasicMessage
message_to_basic_message = |message| {
    role: message.role,
    content: message.content,
    tool_calls: list_to_option(message.tool_calls),
    tool_call_id: str_to_option(message.tool_call_id),
    name: str_to_option(message.name),
}

## Append a system message to the list of messages.
append_system_message : List Message, Str, { cached ?? Bool } -> List Message
append_system_message = |messages, text, { cached ?? Bool.false }|
    List.append(messages, { role: "system", content: text, tool_calls: [], tool_call_id: "", name: "", cached })

## Append a user message to the list of messages.
append_user_message : List Message, Str, { cached ?? Bool } -> List Message
append_user_message = |messages, text, { cached ?? Bool.false }|
    List.append(messages, { role: "user", content: text, tool_calls: [], tool_call_id: "", name: "", cached })

## Append an assistant message to the list of messages.
append_assistant_message : List Message, Str, { cached ?? Bool } -> List Message
append_assistant_message = |messages, text, { cached ?? Bool.false }|
    List.append(messages, { role: "assistant", content: text, tool_calls: [], tool_call_id: "", name: "", cached })
