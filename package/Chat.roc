## The Chat module contains the functions and types needed to use the ChatML formatted chat completion API. It includes the Message type, ChatRequestBody and ChatResponseBody types, and various functions for creating and handling API requests.
module [
    ChatResponseBody,
    Message,
    Client,
    add_assistant_message,
    add_system_message,
    add_user_message,
    build_http_request,
    decode_error_response,
    decode_response,
    update_messages,
    decode_top_message_choice,
    new_client,
]

import json.Json
import json.Option exposing [Option]

import Client
import InternalTools exposing [ToolCall, ToolChoice, Tool]
import Shared exposing [
    ApiError,
    HttpResponse,
    RequestObject,
    drop_leading_garbage,
    option_to_str,
    option_to_list,
    list_to_option,
    str_to_option,
]


Client : Client.Client
Message: Shared.Message

## Internal ChatML message to decode messages from JSON. Allows optional fields.
DecodeMessage : {
    role : Str,
    content : Option Str,
    tool_calls : Option (List ToolCall),
    name : Option Str,
    tool_call_id : Option Str,
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
        text: Option Str,
        id: Option Str,
        name: Option Str,
        # input: Dict Str Str, no decoding ability for Dict
        # TODO: Anthropic tool use blocked on Dict decoding
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

## The structure of the request body to be sent in the Http request.
OpenRouterReqBody : {
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

OpenAIReqBody : {
    model : Str,
    messages : List Message,
    temperature : F32,
    top_p : F32,
    frequency_penalty : F32,
    presence_penalty : F32,
    seed : Option U64,
    max_completion_tokens : Option U64,
    # tools: Option (List Tools.Tool),
    # toolChoice: Option Tools.ToolChoice,
}

AnthropicReqBody : {
    model : Str,
    messages : List Message,
    temperature : F32,
    top_p : F32, 
    top_k : U64, # no openai
    seed : Option U64,
    max_tokens : Option U64,
    system: Option Str,
    # tools: Option (List Tools.Tool),
    # tool_choice: Option ToolChoice,
}

## The structure of non-cached messages to be encoded to JSON for the API.
EncodeBasicMessage : {
    role : Str,
    content : Str,
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

## The message content of a cacheable message.
CacheContent : {
    type : Str,
    text : Str,
    cache_control : Option { type : Str },
}

## Create a request object to be sent with basic-cli's Http.send using ChatML messages.
build_http_request : Client, { tool_choice ?? ToolChoice } -> RequestObject
build_http_request = |client, { tool_choice ?? Auto }|
    tools =
        when Option.get(client.tools) is
            Some(tool_list) -> tool_list
            None -> []
    {
        method: POST,
        headers: get_headers(client),
        uri: get_api_url(client.api),
        body: build_request_body(update_system_message(client, client.messages))
        |> inject_messages(to_compatible_messages(client.messages, client.api))
        |> inject_tools(tools, client.api)
        |> inject_tool_choice(tool_choice, tools, client.api),
        timeout_ms: client.request_timeout,
    }

get_headers = |client|
    when client.api is
        Anthropic -> 
            [
                { name: "content-type", value: "application/json" },
                { name: "anthropic-version", value: "2023-06-01" },
                { name: "x-api-key", value: client.api_key },
            ]
        _ ->
            [
                { name: "content-type", value: "application/json" },
                { name: "Authorization", value: "Bearer ${client.api_key}" },
            ]


get_api_url = |api|
    when api is
        OpenAI -> "https://api.openai.com/v1/chat/completions"
        Anthropic -> "https://api.anthropic.com/v1/messages"
        OpenRouter -> "https://openrouter.ai/api/v1/chat/completions"
        OpenAICompliant { url } -> url

build_request_body = |client|
    when client.api is
        OpenAI -> openai_request_body(client)
        Anthropic -> anthropic_request_body(client)
        OpenRouter -> openrouter_request_body(client)
        OpenAICompliant _ -> openai_request_body(client)

openai_request_body = |client|
    body : OpenAIReqBody
    body = {
        model: client.model,
        messages: [],
        temperature: client.temperature,
        top_p: client.top_p,
        frequency_penalty: client.frequency_penalty,
        presence_penalty: client.presence_penalty,
        seed: client.seed,
        max_completion_tokens: client.max_tokens,
    }
    Encode.to_bytes(
        body,
        Json.utf8_with(
            {
                field_name_mapping: SnakeCase,
                empty_encode_as_null: Json.encode_as_null_option({ record: Bool.false }),
            },
        ),
    )

anthropic_request_body = |client|
    body : AnthropicReqBody
    body = {
        model: client.model,
        messages: [],
        temperature: client.temperature,
        top_p: client.top_p,
        top_k: client.top_k,
        seed: client.seed,
        max_tokens: client.max_tokens,
        system: client.system,
    }
    Encode.to_bytes(
        body,
        Json.utf8_with(
            {
                field_name_mapping: SnakeCase,
                empty_encode_as_null: Json.encode_as_null_option({ record: Bool.false }),
            },
        ),
    )

openrouter_request_body = |client|
    body : OpenRouterReqBody
    body = {
        model: client.model,
        messages: [],
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
    Encode.to_bytes(
        body,
        Json.utf8_with(
            {
                field_name_mapping: SnakeCase,
                empty_encode_as_null: Json.encode_as_null_option({ record: Bool.false }),
            },
        ),
    )

inject_tools : List U8, List Tool, _ -> List U8
inject_tools = |bytes, tools, api|
    if List.is_empty tools then
        bytes
    else
        when api is
            Anthropic -> InternalTools.inject_tools_anthropic(bytes, tools)
            _ -> InternalTools.inject_tools(bytes, tools)

inject_tool_choice = |bytes, tool_choice, tools, api|
    if List.is_empty tools then
        bytes
    else
        when api is
            Anthropic -> InternalTools.inject_tool_choice_anthropic(bytes, tool_choice)
            _ -> InternalTools.inject_tool_choice(bytes, tool_choice)
     
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
                        when bytes is
                            [.., ',', '}', ','] -> List.drop_at(bytes, List.len(bytes) - 3) # drop the last comma before the closing bracket
                            _ -> bytes,
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

## Build a CacheContent object for a message.
build_message_content : Str, Bool -> CacheContent
build_message_content = |text, cached| {
    type: "text",
    text,
    cache_control: if cached then Option.some({ type: "ephemeral" }) else Option.none({}),
}

## TODO: This is very inefficient - plan API changes to move messages into the client. 
## - Then each time a system message is appended when using anthropic, update the system message in the client
update_system_message : Client, List Message -> Client
update_system_message = |client, messages|
    when client.api is
        Anthropic ->
            system = Shared.option_to_str(client.system)
            new_system = List.keep_if(messages, |message| (message.role == "system") and !Str.contains(system, message.content))
                |> List.map .content
                |> List.prepend(system) 
                |> Str.join_with("\n") 
            Client.set_system(client, new_system)
        _ -> client

to_compatible_messages : List Message, Shared.ApiTarget -> List Message
to_compatible_messages = |messages, api| 
    when api is
        Anthropic -> List.drop_if(messages, |message| message.role == "system")
        _ -> messages

## Initialize the OpenRouter API client with the required API key. All parameters besides apiKey are completely optional, and may be set during initialization, assigned later, or left as their defaults.
## ```
## client = Chat.new_client { apiKey: "your_openrouter_api_key" }
## ```
## Same as `Client.new`.
new_client = Client.new

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
                content: Shared.option_to_str(text),
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
update_messages : Client, HttpResponse-> Result Client _
update_messages = |client, response|
    if response.status >= 200 and response.status <= 299 then
        message = decode_top_message_choice(response.body)?
        updated = List.append(client.messages, message)
        Ok({client & messages: updated})
    else
        reponse_body = Str.from_utf8(response.body) |> Result.with_default("")
        Err(HttpError { status: response.status, body: reponse_body })

## Append a system message to the list of messages.
add_system_message : Client, Str, { cached ?? Bool } -> Client
add_system_message = |client, text, { cached ?? Bool.false }|
    when client.api is
        Anthropic ->
            updated = Str.join_with([option_to_str(client.system), text], "\n\n")
            { client & system: Option.some(updated) }
        _ ->
            updated = List.append(client.messages, { role: "system", content: text, tool_calls: [], tool_call_id: "", name: "", cached })
            { client & messages: updated }

## Append a user message to the list of messages.
add_user_message : Client, Str, { cached ?? Bool } -> Client
add_user_message = |client, text, { cached ?? Bool.false }|
    updated = List.append(client.messages, { role: "user", content: text, tool_calls: [], tool_call_id: "", name: "", cached })
    { client & messages: updated }

## Append an assistant message to the list of messages.
add_assistant_message : Client, Str, { cached ?? Bool } -> Client
add_assistant_message = |client, text, { cached ?? Bool.false }|
    updated = List.append(client.messages, { role: "assistant", content: text, tool_calls: [], tool_call_id: "", name: "", cached })
    { client & messages: updated }
