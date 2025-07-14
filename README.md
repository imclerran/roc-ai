# Roc package for building with LLMs

[![Roc-Lang][roc_badge]][roc_link]
[![GitHub last commit][last_commit_badge]][last_commit_link]
[![CI status][ci_status_badge]][ci_status_link]
[![Latest release][version_badge]][version_link]

This package is still in WIP 🛠️ stages, so the interface may be subject to change. With that said, the package currently supports:

> NOTE: roc-ai is currently undergoing a major refactoring. When complete, it should include better support for 
> Anthropic, including function calling, improve encoding performance, and be structured better for long term
> maintainance and support. The goal is to keep public facing interface change minimal, but some interface 
> changes may also be inbound.

- 🚀 __NEW!__ Support for many APIs:
    - Anthropic
    - OpenAI
    - OpenAI compliant, with a custom URL 
        - (Includes local providers, such as Ollama or LM Studio)
    - OpenRouter, with support for hundreds of models, and many OpenRouter exclusive features.
- Creating and parsing ChatML style requests and responses.
- Creating and parsing raw prompt style requests and responses.
- Formatting prompt strings with `[INST]`, `<<SYS>>`, and `<s>` tags for models with llama style fine-tuning.
- Most common LLM parameters such as `temperature`, `top_p`, `top_k`, `repetition_penalty`, etc.
- OpenRouter specific features like fallback models and provider preferences.
- LLM tool use - this enables the AI model to call Roc functions and use the results in its answers.
  - Includes a collection of prebuilt tools, or you can build your own
- Prompt caching on supported models

## Known issues:
- Tool use is currently not supported with the anthropic API, 
    - this is due to missing support in Roc for decoding json dictionaries [roc#5294](https://github.com/roc-lang/roc/issues/5294)
    - __Workaround:__ Anthropic models can be accessed through OpenRouter, with full tool calling support
- Prompt caching has currently only be been tested through OpenRouter


## Example
```roc
main! = |_|
    api_key = Env.var!("OPENAI_API_KEY")?
    client =
        Chat.new_client({ api: OpenAI, api_key, model: "gpt-4o" })
        |> Chat.append_user_message("Hello, computer!", {})
    response = Http.send!(Chat.build_http_request(client, {}))?
    messages = Chat.update_messages(client, response)? |> .messages
    when List.last(messages) is
        Ok(message) -> Stdout.line!(message.content)
        _ -> Ok({})
```

For complete example apps, including a full chatbot app with tool use, see the examples folder.

[roc_badge]: https://img.shields.io/endpoint?url=https%3A%2F%2Fpastebin.com%2Fraw%2FcFzuCCd7
[roc_link]: https://github.com/roc-lang/roc
[ci_status_badge]: https://img.shields.io/github/actions/workflow/status/imclerran/roc-openrouter/ci.yaml?logo=github&logoColor=lightgrey
[ci_status_link]: https://github.com/imclerran/roc-openrouter/actions/workflows/ci.yaml
[last_commit_badge]: https://img.shields.io/github/last-commit/imclerran/roc-openrouter?logo=git&logoColor=lightgrey
[last_commit_link]: https://github.com/imclerran/roc-openrouter/commits/main/
[version_badge]: https://img.shields.io/github/v/release/imclerran/roc-ai
[version_link]: https://github.com/imclerran/roc-ai/releases/
<!--[version_link]: https://github.com/imclerran/roc-ai/releases/latest -->
