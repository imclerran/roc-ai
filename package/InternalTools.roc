module [
    Tool,
    FunctionParameter,
    ToolCall,
    ToolChoice,
    inject_tools,
    inject_tools_anthropic,
    inject_tool_choice,
    inject_tool_choice_anthropic,
    build_tool,
]

import json.Json

## A tool that can be called by the AI model.
Tool : {
    type : Str,
    function : {
        name : Str,
        description : Str,
        parameters : {
            type : Str,
            properties : Dict Str FunctionParameter,
        },
        required : List Str,
    },
}

## A parameter for a tool function.
FunctionParameter : {
    type : Str,
    description : Str,
}

## A call from the model to a tool.
ToolCall : {
    id : Str,
    type : Str,
    function : {
        name : Str,
        arguments : Str,
    },
}

## Set the tool the model should use to process the current message.
ToolChoice : [None, Auto, ToolName Str]

## Inject the tools list into the request body. This is necessary because Encode does not support Dict types.
inject_tools : List U8, List Tool -> List U8
inject_tools = |request_body, tools|
    { before, others } = List.split_at(request_body, ((List.len(request_body)) - 1))
    tools_json = encode_tools(tools)
    [before, (", \"tools\": " |> Str.to_utf8), tools_json, others] |> List.join

inject_tools_anthropic : List U8, List Tool -> List U8
inject_tools_anthropic = |request_body, tools|
    { before, others } = List.split_at(request_body, ((List.len(request_body)) - 1))
    tools_json = encode_tools_anthropic(tools)
    [before, (", \"tools\": " |> Str.to_utf8), tools_json, others] |> List.join

## Inject the tool choice into the request body. This is necessary because Encode does not support Dict types.
inject_tool_choice : List U8, ToolChoice -> List U8
inject_tool_choice = |request_body, tool_choice|
    { before, others } = List.split_at(request_body, ((List.len(request_body)) - 1))
    tool_choice_json =
        when tool_choice is
            None -> "\"none\"" |> Str.to_utf8
            Auto -> "\"auto\"" |> Str.to_utf8
            ToolName(tool_name) ->
                """
                {"type": "function", "function": { "name": "${tool_name}"}}
                """
                |> Str.to_utf8
    [before, (", \"tool_choice\": " |> Str.to_utf8), tool_choice_json, others] |> List.join

inject_tool_choice_anthropic : List U8, ToolChoice -> List U8
inject_tool_choice_anthropic = |request_body, tool_choice|
    { before, others } = List.split_at(request_body, ((List.len(request_body)) - 1))
    when tool_choice is 
        None -> request_body
        Auto ->
            tool_choice_json = "{\"type\": \"auto\"}" |> Str.to_utf8
            [before, (", \"tool_choice\": " |> Str.to_utf8), tool_choice_json, others] |> List.join
        ToolName(tool_name) ->
            tool_choice_json =
                """
                {"type": "function", "function": { "name": "${tool_name}"}}
                """
                |> Str.to_utf8
            [before, (", \"tool_choice\": " |> Str.to_utf8), tool_choice_json, others] |> List.join

## Encode the tools list into JSON UTF-8 bytes
encode_tools : List Tool -> List U8
encode_tools = |tools|
    tools
    |> List.map(tool_to_json)
    |> Str.join_with(", ")
    |> |tools_content| "[${tools_content}]"
    |> Str.to_utf8

encode_tools_anthropic : List Tool -> List U8
encode_tools_anthropic = |tools|
    tools
    |> List.map(tool_to_json_anthropic)
    |> Str.join_with(", ")
    |> |tools_content| "[${tools_content}]"
    |> Str.to_utf8

## Convert a tool object to a JSON string.
tool_to_json : Tool -> Str
tool_to_json = |tool|
    required = Encode.to_bytes(tool.function.required, Json.utf8_with({ field_name_mapping: SnakeCase })) |> Str.from_utf8 |> Result.with_default("[]")
    """
    {"type": "${tool.type}", "function": {"name": "${tool.function.name}", "description": "${tool.function.description}", "parameters": {"type": "${tool.function.parameters.type}", "properties": ${properties_to_json(tool.function.parameters.properties)}}, "required": ${required}}}
    """

tool_to_json_anthropic : Tool -> Str
tool_to_json_anthropic = |tool|
    required = Encode.to_bytes(tool.function.required, Json.utf8_with({ field_name_mapping: SnakeCase })) |> Str.from_utf8 |> Result.with_default("[]")
    """
    {"name": "${tool.function.name}", "description": "${tool.function.description}", "input_schema": {"type": "object", "properties": ${properties_to_json(tool.function.parameters.properties)}, "required": ${required}}}
    """

## Convert a dictionary of function parameters to a JSON string.
properties_to_json : Dict Str FunctionParameter -> Str
properties_to_json = |properties|
    Dict.to_list(properties)
    |> List.map(
        |(param_name, parameter)|
            """
            "${param_name}": {"type": "${parameter.type}", "description": "${parameter.description}"}
            """,
    )
    |> Str.join_with(", ")
    |> |dict_content| "{${dict_content}}"

## Build a tool object.
build_tool : Str, Str, List { name : Str, type : Str, description : Str, required : Bool } -> Tool
build_tool = |name, description, parameters|
    properties =
        parameters
        |> List.map(|{ name: n, type: t, description: d }| (n, { type: t, description: d }))
        |> Dict.from_list
    {
        type: "function",
        function: {
            name,
            description,
            parameters: {
                type: "object",
                properties,
            },
            required: parameters
            # |> List.drop_if(\param -> !param.required)
            |> List.keep_if(.required)
            # |> List.map(\param -> param.name),
            |> List.map(.name),
        },
    }
