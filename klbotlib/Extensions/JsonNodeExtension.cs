#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Text.Json;
using System.Text.Json.Nodes;

namespace klbotlib.Extensions
{
    public static class JsonNodeExtension
    {
        public static string GetString(this JsonNode node, string key)
        {
            var field = node[key].AsValue();
            if (field == null)
                throw new KeyNotFoundException($"Key \"{key}\" not found in JSON object");
            string? value =  (string?)field;
            if (value == null)
                throw new KeyNotFoundException($"Key \"{key}\" is not a value");
            return value;
        }
        public static long GetLong(this JsonNode node, string key)
        {
            var field = node[key];
            if (field == null)
                throw new KeyNotFoundException($"Key \"{key}\" not found in JSON object");
            return (long)node[key].AsValue();
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
}
