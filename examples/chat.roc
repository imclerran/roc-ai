app [main!] {
    cli: platform "../../basic-cli/platform/main.roc",
    ansi: "../../roc-ansi/package/main.roc",
    json: "../../roc-json/package/main.roc",
    ai: "../package/main.roc",
}

import ai.Chat
import ai.Client
import ansi.ANSI as Ansi
import cli.Env
import cli.Http
import cli.Stdin
import cli.Stdout


main! = |_|
    api_key = Env.var!("OPENROUTER_API_KEY")?
    model = get_model_choice!({})?
    provider_order = Dict.get(preferred_providers, model) |> Result.with_default([])
    client = Client.new({ api_key, model, provider_order })
    Stdout.line!("Using model: ${model |> Ansi.color({ fg: Standard(Magenta) })}\n")?
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
    when query |> str_to_lower is
        "goodbye" | "quit" | "exit" -> 
            Ok({})

        "change model" ->
            Stdout.line!("")?
            model = get_model_choice!({})?
            provider_order = Dict.get(preferred_providers, model) |> Result.with_default([])
            new_client = 
                client
                |> Client.set_model(model)
                |> Client.set_provider_order(provider_order)
            Stdout.line!("Using model: ${model |> Ansi.color({ fg: Standard(Magenta) })}\n")?
            loop!(new_client, previous_messages)
        
        _ ->
            req = Chat.build_http_request(client, messages, {})
            req_body = req.body |> Str.from_utf8 |> Result.with_default("")
            dbg req_body
            response = Http.send!(req)?
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

## Prompt the user to choose a model and return the selected model
get_model_choice! : {} => Result Str _
get_model_choice! = |{}|
    Stdout.line!(model_menu_string)?
    Stdout.write!("Choose a model (or press enter): ")?
    choice_str =
        Stdin.line!({})
        |> try
        |> |str| if str == "" then "1" else str
    if Dict.contains(model_choices, choice_str) then
        Dict.get(model_choices, choice_str)
        |> Result.with_default(default_model)
        |> Ok
    else
        "Oops! Invalid model choice.\n" |> Ansi.color({ fg: Standard(Yellow) }) |> Stdout.line! |> try
        get_model_choice!({})

## Initialize the message list with a system message
initialize_messages =
    []
    |> Chat.append_system_message(system_message, {})

system_message =
    """
    You are a helpful assistant, who answers questions in a concise and friendly manner. If you do not have knowledge about the on the users inquires about, you should politely tell them you cannot help."
    """

## The default model selection
default_model = "google/gemini-flash-1.5-8b"

## Define the model choices
model_choices =
    Dict.empty({})
    |> Dict.insert("1", default_model)
    |> Dict.insert("2", "mistralai/mixtral-8x7b-instruct")
    |> Dict.insert("3", "x-ai/grok-beta")
    |> Dict.insert("4", "mistralai/mistral-large")
    |> Dict.insert("5", "gryphe/mythomax-l2-13b")
    |> Dict.insert("6", "microsoft/wizardlm-2-8x22b")
    |> Dict.insert("7", "openai/gpt-3.5-turbo")
    |> Dict.insert("8", "openai/gpt-4o")
    |> Dict.insert("9", "deepseek/deepseek-r1")

## Define the preferred providers for each model
preferred_providers =
    Dict.empty({})
    |> Dict.insert("mistralai/mixtral-8x7b-instruct", ["Fireworks", "Together", "Lepton"])
    |> Dict.insert("gryphe/mythomax-l2-13b", ["DeepInfra", "Fireworks", "Together"])

## Generate a string to print for the model selection menu
model_menu_string =
    model_choices
    |> Dict.walk(
        "",
        |string, key, value|
            string
            |> Str.concat(key)
            |> Str.concat (") ")
            |> Str.concat(value)
            |> Str.concat((if key == "1" then " (default)\n" else "\n")),
    )

## Convert a string to lowercase
str_to_lower : Str -> Str
str_to_lower = |str|
    str
    |> Str.to_utf8
    |> List.walk(
        [],
        |acc, elem|
            acc |> List.append((if elem >= 'a' and elem <= 'z' then elem + 32 else elem)),
    )
    |> Str.from_utf8
    |> Result.with_default(str)
