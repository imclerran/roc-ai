## The prompt module contains functions and types for needed to interact with the OpenRouter API using basic prompt strings. In addition to functions for creating and handling API requests, the module includes functions for formatting prompts for models with Llama-style fine-tuning.
module [
    PromptRequestBody,
    PromptResponseBody,
    Client,
    build_http_request,
    build_request_body,
    decode_error_response,
    decode_error_response,
    decode_response,
    decode_top_text_choice,
    encode_request_body,
    format_llama_prompt,
    format_llama_prompt_with_history,
    new_client,
    update_llama_conversation_history,
]

import json.Json
import json.Option exposing [Option]

import Client
import Shared exposing [RequestObject, ApiError]

Client : Client.Client

## The structure of the request body to be sent in the Http request
PromptRequestBody : {
    prompt : Str,
    model : Str,
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
}

## The structure of the JSON response body received from the OpenRouter API
PromptResponseBody : {
    id : Str,
    model : Str,
    object : Str,
    created : U64,
    choices : List { text : Str, finish_reason : Str },
    usage : {
        prompt_tokens : U64,
        completion_tokens : U64,
        total_tokens : U64,
    },
}

## Initialize the OpenRouter API client with the required API key. All parameters besides apiKey are completely optional, and may be set during initialization, assigned later, or left as their defaults.
## ```
## client = Chat.initClient { apiKey: "your_openrouter_api_key" }
## ```
## Same as `Client.init`.
new_client = Client.new

## Create a request object to be sent with basic-cli's Http.send using a prompt string
build_http_request : Client, Str -> RequestObject
build_http_request = |client, prompt|
    body = build_request_body(client, prompt)
    {
        method: POST,
        headers: [
            { name: "authorization", value: "bearer ${client.api_key}" },
            { name: "content-type", value: "application/json" },
        ],
        uri: Client.get_api_url(client),
        body: encode_request_body(body),
        timeout_ms: client.timeout_ms,
    }

## Build the request body to be sent in the Http request using a prompt string
build_request_body : Client, Str -> PromptRequestBody
build_request_body = |client, prompt| {
    prompt,
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

## Encode the request body to be sent in the Http request
encode_request_body : PromptRequestBody -> List U8
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

## Decode the JSON response to a prompt string request
decode_response : List U8 -> Result PromptResponseBody _
decode_response = |body_bytes|
    cleaned_body = Shared.drop_leading_garbage(body_bytes)
    decoder = Json.utf8_with({ field_name_mapping: SnakeCase })
    decoded : Decode.DecodeResult PromptResponseBody
    decoded = Decode.from_bytes_partial(cleaned_body, decoder)
    decoded.result

## Decode the JSON response body to the first message in the list of choices
decode_top_text_choice : List U8 -> Result Str [ApiError ApiError, DecodingError, NoChoices, BadJson Str]
decode_top_text_choice = |response_body_bytes|
    when decode_response(response_body_bytes) is
        Ok(body) ->
            when List.get(body.choices, 0) is
                Ok(choice) -> Ok(choice.text)
                Err(_) -> Err(NoChoices)

        Err(_) ->
            when decode_error_response(response_body_bytes) is
                Ok(err) -> Err(ApiError(err.error))
                Err(_) ->
                    when response_body_bytes |> Str.from_utf8 is
                        Ok(str) -> Err(BadJson(str))
                        Err(_) -> Err(DecodingError)

## Decode the JSON response body of an API error message
decode_error_response = Shared.decode_error_response

llama_prompt_start_tag = "[INST] "
llama_prompt_end_tag = " [/INST]"
llama_sys_message_start_tag = "<<SYS>>\n"
llama_sys_message_end_tag = "\n<<SYS>>\n\n"
llama_exchange_start_tag = "<s>"
llama_exchange_end_tag = "</s>\n"

## Format the prompt and system message into a Llama-style prompt string.
## ```
## [INST]
## <<SYS>>
## system message here
## <<SYS>>
## prompt here
## [/INST]
## ```
format_llama_prompt : { prompt : Str, sys_message ?? Str } -> Str
format_llama_prompt = |{ prompt, sys_message ?? "" }|
    when sys_message is
        "" ->
            llama_prompt_start_tag
            |> Str.concat(prompt)
            |> Str.concat(llama_prompt_end_tag)

        _ ->
            llama_prompt_start_tag
            |> Str.concat(llama_sys_message_start_tag)
            |> Str.concat(sys_message)
            |> Str.concat(llama_sys_message_end_tag)
            |> Str.concat(prompt)
            |> Str.concat(llama_prompt_end_tag)

## Format the prompt and conversation history into a Llama-style conversation history string.
## ```
## <s>1st exchange</s>
## <s>...</s>
## <s>Nth exchange</s>
## <s>[INST]
## <<SYS>>
## system message here
## <<SYS>>
## prompt here
## [/INST]
## ```
format_llama_prompt_with_history : Str, Str -> Str
format_llama_prompt_with_history = |prompt, conversation_history|
    conversation_history
    |> Str.concat(llama_exchange_start_tag)
    |> Str.concat(prompt)

## Format the most recent prompt and bot reply, and optionally the previous conversation history, into a Llama-style conversation history string.
## ```
## <s>[INST]
## <<SYS>>
## system message here
## <<SYS>>
## first prompt here
## [/INST]first bot reply here</s>
## <s>2nd exchange</s>
## <s>...</s>
## <s>Nth exchange</s>
## ```
update_llama_conversation_history : { prompt_str : Str, bot_reply : Str, conversation_history ?? Str } -> Str
update_llama_conversation_history = |{ prompt_str, bot_reply, conversation_history ?? "" }|
    conversation_history
    |> Str.concat(llama_exchange_start_tag)
    |> Str.concat(prompt_str)
    |> Str.concat(bot_reply)
    |> Str.concat(llama_exchange_end_tag)
