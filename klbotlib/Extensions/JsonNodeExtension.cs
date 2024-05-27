#pragma warning disable CS1591
using System.Collections.Generic;
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
            var field = node[key].AsValue();
            if (field == null)
                throw new KeyNotFoundException($"Key \"{key}\" not found in JSON object");
            return (long)field;
        }
    }
}
