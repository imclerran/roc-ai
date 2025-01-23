app [main!] {
    cli: platform "../../basic-cli/platform/main.roc",
    ansi: "../../roc-ansi/package/main.roc",
    json: "../../roc-json/package/main.roc",
    ai: "../package/main.roc",
}

import ai.Chat
import ai.Client exposing [Client]
import ansi.ANSI as Ansi
import cli.Env
import cli.Http
import cli.Stdin
import cli.Stdout

main! = |_|
    client = get_client!({})?
    Stdout.line!("Using model: ${client.model |> Ansi.color({ fg: Standard(Magenta) })}")?
    Stdout.line!("From: ${client.api |> api_to_str |> Ansi.color({ fg: Standard(Magenta) })}\n")?
    Stdout.line!("Enter your questions below, or type 'Goodbye' to exit")?
    loop!(client, initialize_messages)?
    "\nAssistant:  I have been a good chatbot. Goodbye! 😊"
    |> Ansi.color({ fg: Standard(Magenta) })
    |> Stdout.line!?
    Ok({})

loop! = |client, previous_messages|
    Stdout.write!("You: ")?
    query = Stdin.line!({})?
    messages = Chat.append_user_message(previous_messages, query, {})
    response = Http.send!(Chat.build_http_request(client, messages, {}))?
    updated_messages = Chat.update_message_list(response, messages)
    print_last_message!(updated_messages)?
    loop!(client, updated_messages)

# Print the last message in the list of messages. Will only print assistant and system messages.
print_last_message! = |messages|
    when List.last(messages) is
        Ok({ role, content }) if role == "assistant" ->
            Stdout.line! (("\nAssistant: ${content}\n" |> Ansi.color({ fg: Standard(Magenta) })))

        Ok({ role, content }) if role == "system" ->
            Stdout.line! (("\nSystem: ${content}\n" |> Ansi.color({ fg: Standard(Blue) })))

        _ -> Ok({})

## Initialize the message list with a system message
initialize_messages =
    []
    |> Chat.append_system_message(
        """
        You are a helpful assistant, who answers questions in a concise and friendly manner. If you do not have knowledge about the on the users inquires about, you should politely tell them you cannot help."
        """,
        {},
    )

## Define the preferred providers for each model
preferred_providers = Dict.empty({}) |> Dict.insert("deepseek/deepseek-r1", ["Fireworks", "Together"])

# Add these constants near other constants
api_choices =
    Dict.empty({})
    |> Dict.insert("1", { api: OpenAI, model: "gpt-4o-mini" })
    |> Dict.insert("2", { api: Anthropic, model: "claude-3-5-sonnet-20241022" })
    |> Dict.insert("3", { api: OpenRouter, model: "gpt-4o-mini" })
    |> Dict.insert("4", { api: OpenRouter, model: "anthropic/claude-3.5-sonnet:beta" })
    |> Dict.insert("5", { api: OpenRouter, model: "deepseek/deepseek-r1" })
    |> Dict.insert("6", { api: OpenAICompliant { url: "http://127.0.0.1:1234/v1/chat/completions" }, model: "deepseek-r1-distill-qwen-1.5b" })

api_to_str = |api|
    when api is
        OpenAI -> "OpenAI"
        Anthropic -> "Anthropic"
        OpenRouter -> "OpenRouter"
        OpenAICompliant _ -> "LM Studio"

api_menu_string =
    api_choices
    |> Dict.walk(
        "",
        |string, key, value|
            provider_str = api_to_str(value.api)
            string
            |> Str.concat(key)
            |> Str.concat(") ")
            |> Str.concat("${provider_str}: ")
            |> Str.concat(value.model)
            |> Str.concat((if key == "1" then " (default)\n" else "\n")),
    )

get_client! : {} => Result Client _
get_client! = |{}|
    Stdout.line!(api_menu_string)?
    Stdout.write!("Choose an API (or press enter): ")?
    choice_str =
        Stdin.line!({})
        |> try
        |> |str| if str == "" then "1" else str

    when Dict.get(api_choices, choice_str) is
        Ok({ api: OpenAI, model }) ->
            api_key = Env.var!("OPENAI_API_KEY")?
            Ok(Client.new({ api: OpenAI, api_key, model }))

        Ok({ api: Anthropic, model }) ->
            api_key = Env.var!("ANTHROPIC_API_KEY")?
            Ok(Client.new({ api: Anthropic, api_key, model, max_tokens: 4096 }))

        Ok({ api: OpenRouter, model }) ->
            provider_order = Dict.get(preferred_providers, model) |> Result.with_default([])
            api_key = Env.var!("OPENROUTER_API_KEY")?
            Ok(Client.new({ api: OpenRouter, api_key, model, provider_order }))

        Ok({ api: OpenAICompliant { url }, model }) ->
            Ok(Client.new({ api: OpenAICompliant { url }, api_key: "", model }))

        Err _ ->
            "Oops! Invalid API choice.\n"
            |> Ansi.color({ fg: Standard(Yellow) })
            |> Stdout.line!?
            get_client!({})
