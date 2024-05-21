using System.Text.Json.Nodes;

namespace klbotlib.MessageClient.OneBot.JsonPrototypes;

internal record JOneBotMessageContent(string? Type, JsonObject? Data);
