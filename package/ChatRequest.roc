module [build_http_request]

import json.Option exposing [Option]
import json.Json
import Shared exposing [
    RequestObject, 
    Message,
    list_to_option,
    str_to_option,
]

import InternalTools exposing [ToolCall, ToolChoice, Tool]
import Client exposing [Client]

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
    # toolChoice: Option Tools.ToolChoice,
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
build_http_request : Client, List Message, { tool_choice ?? ToolChoice } -> RequestObject
build_http_request = |client, messages, { tool_choice ?? Auto }|
    tools =
        when Option.get(client.tools) is
            Some(tool_list) -> tool_list
            None -> []
    {
        method: POST,
        headers: get_headers(client),
        uri: get_api_url(client.api),
        body: build_request_body(update_system_message(client, messages))
        |> inject_messages(compatible_messages(messages, client.api))
        |> inject_tools(tools)
        |> inject_tool_choice(tool_choice, tools),
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

inject_tools : List U8, List Tool -> List U8
inject_tools = |bytes, tools|
    if List.is_empty tools then
        bytes
    else
        InternalTools.inject_tools(bytes, tools)

inject_tool_choice = |bytes, tool_choice, tools|
    if List.is_empty tools then
        bytes
    else
        InternalTools.inject_tool_choice(bytes, tool_choice)
     
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

compatible_messages : List Message, Shared.ApiTarget -> List Message
compatible_messages = |messages, api| 
    when api is
        Anthropic -> List.drop_if(messages, |message| message.role == "system")
        _ -> messages