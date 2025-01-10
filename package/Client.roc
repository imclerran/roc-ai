## Client for the OpenRouter.ai API. This module contains the Client object, which stores configuration for openrouter.ai API requrests, as well as the new function, and functions to set various configuration options.
module [
    Client,
    new,
    set_model,
    set_url,
    set_request_timeout,
    set_provider_order,
    set_temperature,
    set_top_p,
    set_top_k,
    set_frequency_penalty,
    set_presence_penalty,
    set_repetition_penalty,
    set_min_p,
    set_top_a,
    set_seed,
    set_max_tokens,
    set_models,
    set_route,
    set_tools,
    default_model,
    default_url,
]

import json.Option exposing [Option]
import Shared exposing [TimeoutConfig]
import InternalTools exposing [Tool]

## The record used to store configuration for the OpenRouter API client.
## ```
## Client : {
##     apiKey : Str,
##     model : Str,
##     url : Str,
##     requestTimeout : TimeoutConfig,
##     providerOrder : Option (List Str),
##     temperature : F32,
##     topP : F32,
##     topK : U64,
##     frequencyPenalty : F32,
##     presencePenalty : F32,
##     repetitionPenalty : F32,
##     minP : F32,
##     topA : F32,
##     seed : Option U64,
##     maxTokens : Option U64,
##     models : Option (List Str),
##     route : Option Str,
##     tools: Option (List Tool),
## }
## ```
Client : {
    api_key : Str,
    model : Str,
    url : Str,
    request_timeout : TimeoutConfig,
    provider_order : Option (List Str),
    temperature : F32,
    top_p : F32,
    top_k : U64,
    frequency_penalty : F32,
    presence_penalty : F32,
    repetition_penalty : F32,
    min_p : F32,
    top_a : F32,
    seed : Option U64,
    max_tokens : Option U64,
    # responseFormat : { type : Str },
    models : Option (List Str),
    route : Option Str,
    tools : Option (List Tool),
}

## Default model to use for API requests. This defaults to the openrouter/auto model router.
default_model = "openrouter/auto"

## The default URL for the OpenRouter API. Currently the only supported URL is the openrouter.ai API url.
default_url = "https://openrouter.ai/api/v1/chat/completions"

## Initialize the OpenRouter API client with the required API key. All parameters besides apiKey are completely optional, and may be set during newialization, assigned later, or left as their defaults.
## ```
## client = Client.new { apiKey: "your_openrouter_api_key" }
## ```
new :
    {
        api_key : Str,
        model ?? Str,
        url ?? Str,
        request_timeout ?? TimeoutConfig,
        provider_order ?? List Str,
        temperature ?? F32,
        top_p ?? F32,
        top_k ?? U64,
        frequency_penalty ?? F32,
        presence_penalty ?? F32,
        repetition_penalty ?? F32,
        min_p ?? F32,
        top_a ?? F32,
        seed ?? U64,
        max_tokens ?? U64,
        models ?? List Str,
        route ?? [UseFallback, NoFallback],
        tools ?? List Tool,
    }
    -> Client
new = \{ api_key, model ?? default_model, url ?? default_url, request_timeout ?? NoTimeout, provider_order ?? [], temperature ?? 1.0, top_p ?? 1.0, top_k ?? 0, frequency_penalty ?? 0.0, presence_penalty ?? 0.0, repetition_penalty ?? 1.0, min_p ?? 0.0, top_a ?? 0.0, seed ?? 0, max_tokens ?? 0, models ?? [], route ?? NoFallback, tools ?? [] } ->
    {
        api_key,
        model,
        url,
        request_timeout,
        provider_order: Option.none({}),
        temperature,
        top_p,
        top_k,
        frequency_penalty,
        presence_penalty,
        repetition_penalty,
        min_p,
        top_a,
        seed: Option.none({}),
        max_tokens: Option.none({}),
        models: Option.none({}),
        route: Option.none({}),
        tools: Option.none({}),
    }
    |> set_provider_order(provider_order)
    |> set_seed(seed)
    |> set_max_tokens(max_tokens)
    |> set_models(models)
    |> set_route(route)
    |> set_tools(tools)

## Set the model to be used for the API requests.
## Default: "openrouter/auto"
set_model : Client, Str -> Client
set_model = \client, model -> { client & model }

## Set the URL to be used for the API requests. (Change with care - while the openrouter.ai API is similar to OpenAI's, there may be some unexpected differences.)
set_url : Client, Str -> Client
set_url = \client, url -> { client & url }

## Set the request timeout for the API requests.
## Default: NoTimeout
set_request_timeout : Client, TimeoutConfig -> Client
set_request_timeout = \client, request_timeout -> { client & request_timeout }

## Set the provider order for the API requests.
## Default: [] - use all providers.
set_provider_order : Client, List Str -> Client
set_provider_order = \client, provider_order ->
    provider_order_option =
        when provider_order is
            [] -> Option.none({})
            [..] -> Option.some(provider_order)
    { client & provider_order: provider_order_option }

## Set the temperature for the API requests.
## Range: [0.0, 2.0]
## Default: 1.0
set_temperature : Client, F32 -> Client
set_temperature = \client, temperature -> { client & temperature }

## Set the top_p for the API requests.
## Range: [0.0, 1.0]
## Default: 1.0
set_top_p : Client, F32 -> Client
set_top_p = \client, top_p -> { client & top_p }

## Set the top_k for the API requests.
## Range: [0, Num.maxU64]
## Default: 0
set_top_k : Client, U64 -> Client
set_top_k = \client, top_k -> { client & top_k }

## Set the frequency penalty for the API requests.
## Range: [-2.0, 2.0]
## Default: 0.0
set_frequency_penalty : Client, F32 -> Client
set_frequency_penalty = \client, frequency_penalty -> { client & frequency_penalty }

## Set the presence penalty for the API requests.
## Range: [-2.0, 2.0]
## Default: 0.0
set_presence_penalty : Client, F32 -> Client
set_presence_penalty = \client, presence_penalty -> { client & presence_penalty }

## Set the repetition penalty for the API requests.
## Range: [0.0, 2.0]
## Default: 1.0
set_repetition_penalty : Client, F32 -> Client
set_repetition_penalty = \client, repetition_penalty -> { client & repetition_penalty }

## Set the min_p for the API requests.
## Range: [0.0, 1.0]
## Default: 0.0
set_min_p : Client, F32 -> Client
set_min_p = \client, min_p -> { client & min_p }

## Set the top_a for the API requests.
## Range: [0.0, 1.0]
## Default: 0.0
set_top_a : Client, F32 -> Client
set_top_a = \client, top_a -> { client & top_a }

## Set the seed for the API requests. (This is for OpenAI models only)
## Default: 0 - random seed
set_seed : Client, U64 -> Client
set_seed = \client, seed ->
    seed_option =
        when seed is
            0 -> Option.none({})
            _ -> Option.some(seed)
    { client & seed: seed_option }

## Set the max_tokens for the API requests.
## Range: [1, contextLength]
## Default: 0 == no limit
set_max_tokens : Client, U64 -> Client
set_max_tokens = \client, max_tokens ->
    max_tokens_option =
        when max_tokens is
            0 -> Option.none({})
            _ -> Option.some(max_tokens)
    { client & max_tokens: max_tokens_option }

## Set the response format to either "text" or "json_object". Not supported by all models.
## Default: "" - no format
# setResponseFormat : Client, Str -> Client
# setResponseFormat = \client, responseFormat ->
#     responseFormatRecord = { type: responseFormat }
#     { client & responseFormat: responseFormatRecord }

## Set the models for the auto router to choose from. If not set, the auto router will choose from a small selection of the top performing models.
## https://openrouter.ai/models/openrouter/auto
## Default: []
set_models : Client, List Str -> Client
set_models = \client, models ->
    models_option =
        if
            List.is_empty(models)
        then
            Option.none({})
            else

        Option.some(models)
    { client & models: models_option }

## Set the parameter which determines whether to use a fallback model if the primary model fails. OpenRouter will use the models provided in models, or if no models are provided, will try a similarly priced model to the primary.
## https://openrouter.ai/docs#model-routing
## Default: NoFallback
set_route : Client, [UseFallback, NoFallback] -> Client
set_route = \client, route ->
    route_option =
        when route is
            NoFallback -> Option.none({})
            UseFallback -> Option.some("fallback")
    { client & route: route_option }

## Set the list of tools available for models to use to handle requests.
## Default: []
set_tools : Client, List Tool -> Client
set_tools = \client, tools ->
    tools_option =
        if
            List.is_empty(tools)
        then
            Option.none({})
        else
            Option.some(tools)
    { client & tools: tools_option }
