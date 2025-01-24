app [main!] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/bi5zubJ-_Hva9vxxPq4kNx4WHX6oFs8OP6Ad0tCYlrY.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Http
import cli.Env
import ai.Prompt
import ai.Client

main! = |_|
    api_key = Env.var!("OPENROUTER_API_KEY")?
    client =
        Client.new({ api_key })
        |> Client.set_model("mistralai/mixtral-8x7b-instruct")
        |> Client.set_provider_order(["Fireworks", "Together", "Lepton"])
        |> Client.set_temperature(0.0)
        |> Client.set_top_p(1.0)
        |> Client.set_max_tokens(8)
    query = Prompt.format_llama_prompt({ prompt: "Hello, computer!" })
    response = Http.send!(Prompt.build_http_request(client, query))?
    when Prompt.decode_top_text_choice(response.body) is
        Ok(text) -> Stdout.line!((text |> Str.trim))
        Err(ApiError(error)) -> Stdout.line!(error.message)
        Err(NoChoices) -> Stdout.line!("No choices found in API response")
        Err(BadJson(str)) -> Stdout.line!("Invalid JSON response:\n${str}")
        Err(DecodingError) -> Stdout.line!("Invalid API response")

