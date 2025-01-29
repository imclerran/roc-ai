app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br",
    ansi: "https://github.com/lukewilliamboswell/roc-ansi/releases/download/0.8.0/RQlGWlkQEfxtkSYKl0nHNQaOFT0-Jh7NNFEX2IPXlec.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Stdin
import cli.Http
import cli.Env

import ai.Chat exposing [Message]
import ai.Tools { send_http_req!: Http.send! }
import ai.Toolkit.OpenWeatherMap { send_http_req!: Http.send!, get_env_var!: Env.var! } exposing [geocoding, current_weather]
import ai.Toolkit.Serper { send_http_req!: Http.send!, get_env_var!: Env.var! } exposing [serper]
import ansi.ANSI as Ansi

main! = |_|
    api_key = Env.var!("OPENAI_API_KEY")?
    client = Chat.new_client({ api: OpenAI, api_key, model: "gpt-4o", tools: [geocoding.tool, current_weather.tool, serper.tool] })
    Stdout.line!(("Assistant: Ask me about the weather or stock prices!\n" |> Ansi.color({ fg: Standard(Cyan) })))?
    loop!(client)

loop! : Chat.Client => Result {} _
loop! = |client|
    Stdout.write!("You: ")?
    with_query = Chat.add_user_message(client, Stdin.line!({})?, {})
    response = Http.send!(Chat.build_http_request(with_query, {}))?
    with_response = Chat.update_messages(with_query, response)?
    with_final_answer = Tools.handle_tool_calls!(with_response, tool_handler_map, { max_model_calls: 10 })?
    print_last_message!(with_final_answer.messages)?
    loop!(with_final_answer)

print_last_message! : List Message => Result {} _
print_last_message! = |messages|
    when List.last(messages) is
        Ok({ role, content }) if role == "assistant" ->
            Stdout.line!(("\nAssistant: ${content}\n" |> Ansi.color({ fg: Standard(Cyan) })))

        _ -> Ok({})

tool_handler_map : Dict Str (Str => Result Str _)
tool_handler_map =
    Dict.from_list(
        [
            (geocoding.name, geocoding.handler!),
            (current_weather.name, current_weather.handler!),
            (serper.name, serper.handler!),
        ],
    )
