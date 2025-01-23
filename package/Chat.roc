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
import InternalTools exposing [ToolCall]
import Shared exposing [
    ApiError,
    HttpResponse,
    drop_leading_garbage,
    option_to_str,
    option_to_list,
]
import ChatRequest

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

## The structure of the request body to be sent in the Http request.
ChatRequestBody : {
    model : Str,
    messages : List Message,
    temperature : F32,
    # top_a : F32, # no anthropic
    top_p : F32, 
    # top_k : U64, # no openai
    # frequency_penalty : F32, # no anthropic
    # presence_penalty : F32, # no anthropic
    # repetition_penalty : F32, # no anthropic
    # min_p : F32, # no anthropic
    seed : Option U64,
    max_tokens : Option U64,
    # provider : { # no anthropic
    #     order : Option (List Str),
    # },
    # models : Option (List Str), # no anthropic
    # route : Option Str, # no anthropic
    # tools: Option (List Tools.Tool),
    # toolChoice: Option Tools.ToolChoice,
    system: Option Str,
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

DecodeAnthropicResponseBody : {
    id: Str,
    model: Str,
    type: Str,
    role: Str,
    content: List {
        type: Str,
        text: Str,
    },
    stop_reason: Str,
    # stop_sequence: ?
    usage: {
        input_tokens: U64,
        cache_creation_input_tokens: U64,
        cache_read_input_tokens: U64,
        output_tokens: U64,
    }
}

## Initialize the OpenRouter API client with the required API key. All parameters besides apiKey are completely optional, and may be set during initialization, assigned later, or left as their defaults.
## ```
## client = Chat.new_client { apiKey: "your_openrouter_api_key" }
## ```
## Same as `Client.new`.
new_client = Client.new

## Create a request object to be sent with basic-cli's Http.send using ChatML messages.
# build_http_request : Client, List Message, { tool_choice ?? ToolChoice } -> RequestObject
# build_http_request = |client, messages, { tool_choice ?? Auto }|
#     body = build_request_body(client)
#     tools =
#         when Option.get(client.tools) is
#             Some(tool_list) -> tool_list
#             None -> []
#     {
#         method: POST,
#         headers: [
#             { name: "Authorization", value: "Bearer ${client.api_key}" },
#             { name: "anthropic-version", value: "2023-06-01" },
#             { name: "x-api-key", value: client.api_key },
#             { name: "content-type", value: "application/json" },
#         ],
#         uri: client.url,
#         body: encode_request_body(body)
#         |> inject_messages(messages)
#         |> inject_tools(tools)
#         |> inject_tool_choice(tool_choice, tools),
#         timeout_ms: client.request_timeout,
#     }
build_http_request = ChatRequest.build_http_request

## Decode the JSON response body to a ChatML style request.
decode_response : List U8 -> Result ChatResponseBody _
decode_response = |body_bytes|
    cleaned_body = drop_leading_garbage(body_bytes)
    when decode_default_response(cleaned_body) is
        Ok response -> Ok response
        Err _ -> decode_anthropic_response(cleaned_body)

decode_default_response : List U8 -> Result ChatResponseBody _
decode_default_response = |body_bytes|
    decoder = Json.utf8_with({ field_name_mapping: SnakeCase })
    decoded : Decode.DecodeResult DecodeChatResponseBody
    decoded = Decode.from_bytes_partial(body_bytes, decoder)
    decoded.result |> Result.map_ok(from_decode_response)

decode_anthropic_response : List U8 -> Result ChatResponseBody _
decode_anthropic_response = |body_bytes|
    decoder = Json.utf8_with({ field_name_mapping: SnakeCase })
    decoded : Decode.DecodeResult DecodeAnthropicResponseBody
    decoded = Decode.from_bytes_partial(body_bytes, decoder)
    decoded.result |> Result.map_ok(from_anthropic_response)

from_decode_response : DecodeChatResponseBody -> ChatResponseBody
from_decode_response = |internal_response| 
    {
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
    }

from_anthropic_response : DecodeAnthropicResponseBody -> ChatResponseBody
from_anthropic_response = |internal_response|
    {
        id: internal_response.id,
        model: internal_response.model,
        object: internal_response.type,
        created: 0,
        choices: choices_from_anthropic(internal_response),
        usage: {
            prompt_tokens: internal_response.usage.input_tokens,
            completion_tokens: internal_response.usage.output_tokens,
            total_tokens: internal_response.usage.input_tokens + internal_response.usage.output_tokens,
        }
    }

choices_from_anthropic : DecodeAnthropicResponseBody -> List { index: U8, message: Message, finish_reason: Str }
choices_from_anthropic = |response| 
    response.content |> List.map_with_index(
        |{ type: _, text }, index| {
            index: Num.to_u8(index),
            message: {
                role: "assistant",
                content: text,
                tool_calls: [],
                name: "",
                tool_call_id: "",
                cached: Bool.false,
            },
            finish_reason: response.stop_reason,
        }
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
