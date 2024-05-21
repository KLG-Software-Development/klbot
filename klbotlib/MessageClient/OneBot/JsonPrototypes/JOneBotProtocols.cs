namespace klbotlib.MessageClient.OneBot.JsonPrototypes;

internal record JOneBotEvent(JOneBotSelf? Self, string? Id, double Time, string? Type, string? DetailType, string? SubType);
internal record JOneBotAction<T>(JOneBotSelf? Self, string? Action, T? Params, string? Echo);
internal record JOneBotResponse<T>(JOneBotSelf? Self, string? Status, long Retcode, T? Data, string? Message, string? Echo);
