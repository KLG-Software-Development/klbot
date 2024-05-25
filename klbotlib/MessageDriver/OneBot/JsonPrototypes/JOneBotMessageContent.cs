using System.Text.Json.Nodes;

namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

internal record JOneBotMessageContent(string? Type, JsonObject? Data);
