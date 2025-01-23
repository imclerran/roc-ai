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
api_choices = [
    { api: OpenAI, model: "gpt-4o-mini" },
    { api: Anthropic, model: "claude-3-5-sonnet-20241022" },
    { api: OpenRouter, model: "gpt-4o-mini" },
    { api: OpenRouter, model: "anthropic/claude-3.5-sonnet:beta" },
    { api: OpenRouter, model: "deepseek/deepseek-r1" },
    { api: OpenAICompliant { url: "http://127.0.0.1:1234/v1/chat/completions" }, model: "deepseek-r1-distill-qwen-1.5b" },
]

api_to_str = |api|
    when api is
        OpenAI -> "OpenAI"
        Anthropic -> "Anthropic"
        OpenRouter -> "OpenRouter"
        OpenAICompliant _ -> "LM Studio"

api_menu_string =
    api_choices
    |> List.walk_with_index(
        "",
        |string, value, index|
            provider_str = api_to_str(value.api)
            colorize = color_by_number(index)
            string
            |> Str.concat(Num.to_str(index + 1))
            |> Str.concat(") ")
            |> Str.concat(colorize("${provider_str}: ${value.model}"))
            |> Str.concat((if index == 0 then " (default)\n" else "\n")),
    )

get_client! : {} => Result Client _
get_client! = |{}|
    Stdout.line!(api_menu_string)?
    Stdout.write!("Choose a Model (or press enter): ")?
    choice =
        Stdin.line!({})?
        |> |str| if str == "" then "1" else str
        |> Str.to_u64
        |> Result.with_default(0)
        |> Num.sub_wrap(1)

    when List.get(api_choices, choice) is
        Ok({ api: OpenAI, model }) ->
            api_key = Env.var!("OPENAI_API_KEY")?
            print_choice!(choice, OpenAI, model)?
            Ok(Client.new({ api: OpenAI, api_key, model }))

        Ok({ api: Anthropic, model }) ->
            api_key = Env.var!("ANTHROPIC_API_KEY")?
            print_choice!(choice, Anthropic, model)?
            Ok(Client.new({ api: Anthropic, api_key, model, max_tokens: 4096 }))

        Ok({ api: OpenRouter, model }) ->
            provider_order = Dict.get(preferred_providers, model) |> Result.with_default([])
            api_key = Env.var!("OPENROUTER_API_KEY")?
            print_choice!(choice, OpenRouter, model)?
            Ok(Client.new({ api: OpenRouter, api_key, model, provider_order }))

        Ok({ api: OpenAICompliant { url }, model }) ->
            print_choice!(choice, OpenAICompliant { url }, model)?
            Ok(Client.new({ api: OpenAICompliant { url }, api_key: "", model }))

        Err _ ->
            "Oops! Invalid API choice.\n"
            |> Ansi.color({ fg: Standard(Yellow) })
            |> Stdout.line!?
            get_client!({})

color_by_number = |n|
    when (n % 6) is
        0 -> |str| Ansi.color(str, { fg: Standard(Red) })
        1 -> |str| Ansi.color(str, { fg: Standard(Yellow) })
        2 -> |str| Ansi.color(str, { fg: Standard(Green) })
        3 -> |str| Ansi.color(str, { fg: Standard(Cyan) })
        4 -> |str| Ansi.color(str, { fg: Standard(Blue) })
        _ -> |str| Ansi.color(str, { fg: Standard(Magenta) })

print_choice! = |n, api, model|
    colorize = color_by_number(n)
    Stdout.line!("Using model: ${model |> colorize}")?
    Stdout.line!("From: ${api |> api_to_str |> colorize}\n")
