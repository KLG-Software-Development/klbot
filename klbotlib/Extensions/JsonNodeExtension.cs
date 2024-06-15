#pragma warning disable CS1591
using System.Text.Json;
using System.Text.Json.Nodes;

namespace klbotlib.Extensions;

public static class JsonNodeExtension
{
    public static string GetString(this JsonNode node, string key)
    {
        var rawValue = node[key] ?? throw new KeyNotFoundException($"Key \"{key}\" not found in JSON object");
        var field = rawValue.AsValue() ?? throw new KeyNotFoundException($"Key \"{key}\" not found in JSON object");
        string? value = (string?)field;
        return value ?? throw new KeyNotFoundException($"Key \"{key}\" is not a value");
    }
    public static long GetLong(this JsonNode node, string key)
    {
        var field = node[key] ?? throw new KeyNotFoundException($"Key \"{key}\" not found in JSON object");
        return (long)field.AsValue();
    }
    public static Dictionary<string, object?> ToDict(this JsonNode node)
    {
        Dictionary<string, object?> dict = [];
        var kind = node.GetValueKind();
        if (kind != JsonValueKind.Object)
            throw new JsonException($"Cannot converting value kind [{kind}] to object dictionary");
        JsonObject jobj = node.AsObject();
        foreach (var kvp in jobj)
        {

            if (!dict.TryAdd(kvp.Key, kvp.Value))
                throw new Exception($"Failed to insert key [{kvp.Key}]");
        }
        return dict;
    }
}
