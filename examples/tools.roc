app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
    ansi: "../../roc-ansi/package/main.roc",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Stdin
import cli.Http
import cli.Env

import ai.Chat
import ai.Tools { send_http_req!: Http.send! }
import ai.Toolkit.OpenWeatherMap { send_http_req!: Http.send!, get_env_var!: Env.var! } exposing [geocoding, current_weather]
import ai.Toolkit.Serper { send_http_req!: Http.send!, get_env_var!: Env.var! } exposing [serper]
import ansi.ANSI as Ansi

main! = |_|
    api_key = Env.var!("OPENROUTER_API_KEY")?
    client = Chat.new_client({ api_key, model: "openai/gpt-4o", tools: [geocoding.tool, current_weather.tool, serper.tool] })
    Stdout.line!(("Assistant: Ask me about the weather, or anything on the web!\n" |> Ansi.color({ fg: Standard(Cyan) })))?
    loop!(client, [])
    
loop! : Chat.Client, List Chat.Message => Result {} _
loop! = |client, previous_messages|
    Stdout.write!("You: ")?
    messages = Chat.append_user_message(previous_messages, Stdin.line!({})?, {})
    response = Http.send!(Chat.build_http_request(client, messages, {}))?
    messages2 = Chat.update_message_list(response, messages)
    messages3 = Tools.handle_tool_calls!(messages2, client, tool_handler_map, { max_model_calls: 10 })?
    print_last_message!(messages3)?
    loop!( client, messages3 )

# Print the last message in the list of messages. Will only print assistant and system messages.
# print_last_message : List Message => Result {} _
print_last_message! = |messages|
    when List.last(messages) is
        Ok({ role, content }) if role == "assistant" ->
            Stdout.line!(("\nAssistant: ${content}\n" |> Ansi.color({ fg: Standard(Magenta) })))

        Ok({ role, content }) if role == "system" ->
            Stdout.line!(("\nAssistant: ${content}\n" |> Ansi.color({ fg: Standard(Cyan) })))

        _ -> Ok({})

## Map of tool names to tool handlers
tool_handler_map : Dict Str (Str => Result Str _)
tool_handler_map =
    Dict.from_list(
        [
            (geocoding.name, geocoding.handler!),
            (current_weather.name, current_weather.handler!),
            (serper.name, serper.handler!),
        ],
    )
