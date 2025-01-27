app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
    ai: "https://github.com/imclerran/roc-ai/releases/download/v0.9.0/LIMYIvGIjaL4cbOXI6mgjV5pQEdNVE5w_G8ggz1uxGU.tar.br",
}

import ai.Chat
import cli.Env
import cli.Http
import cli.Stdout

main! = |_|
    api_key = Env.var!("OPENAI_API_KEY")?
    client = 
        Chat.new_client({ api: OpenAI, model: "gpt-4o-mini",  api_key })
        |> Chat.append_user_message("Hello, computer!", {})
    response = Http.send!(Chat.build_http_request(client, {}))?
    messages = Chat.update_messages(client, response)? |> .messages
    when List.last(messages) is
        Ok(message) -> Stdout.line!(message.content)
        _ -> Ok({})