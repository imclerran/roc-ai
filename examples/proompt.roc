app [main] {
    cli: platform "https://github.com/roc-lang/basic-cli/releases/download/0.10.0/vNe6s9hWzoTZtFmNkvEICPErI9ptji_ySjicO6CkucY.tar.br",
    json: "https://github.com/lukewilliamboswell/roc-json/releases/download/0.10.0/KbIfTNbxShRX1A1FgXei1SpO5Jn8sgP6HP6PXbi-xyA.tar.br",
    ai: "../package/main.roc",
}

import cli.Stdout
import cli.Http
import cli.Task exposing [Task]
import cli.Env
import ai.Api as Api
import ai.Client

main =
    apiKey = getApiKey!
    client =
        Client.init { apiKey }
        |> Client.setModel "mistralai/mixtral-8x7b-instruct"
        |> Client.setProviderOrder ["Fireworks", "Together", "Lepton"]
        |> Client.setTemperature 0.0
        |> Client.setTopP 1.0
        |> Client.setMaxTokens 8
    query = Api.formatLLamaPromptStr { prompt: "Hello, world!" }
    response = Http.send! (Api.buildPromptRequest client query)
    responseBody =
        when response |> Http.handleStringResponse is
            Err err -> crash (Http.errorToString err)
            Ok body -> body |> Str.toUtf8

    when Api.decodePromptResponse responseBody is
        Ok body ->
            when List.first body.choices is
                Ok choice -> Stdout.line (choice.text |> Str.trim)
                Err _ -> Stdout.line "No choices found in API response"

        Err _ ->
            when Api.decodeErrorResponse responseBody is
                Ok { error } -> Stdout.line error.message
                Err _ -> Stdout.line "Failed to decode API response"

## Get the API key from the environmental variable
getApiKey =
    keyResult <- Task.attempt (Env.var "OPENROUTER_API_KEY")
    when keyResult is
        Ok key -> Task.ok key
        Err VarNotFound -> crash "OPENROUTER_API_KEY environment variable not set"
