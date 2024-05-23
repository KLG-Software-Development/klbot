namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

internal record JOneBotEvent(long Time, string? SelfId, string? PostType)
{
    public static JOneBotEvent Empty { get; } = new(0, null, null);
}
internal record JOneBotAction<T>(JOneBotSelf? Self, string? Action, T? Params, string? Echo);
internal record JOneBotResponse<T>(JOneBotSelf? Self, string? Status, long Retcode, T? Data, string? Message, string? Echo);

internal record JOneBotEvent_Message(long Time, string? SelfId, string? PostType)
    : JOneBotEvent(Time, SelfId, PostType);
internal record JOneBotEvent_Notice(long Time, string? SelfId, string? PostType)
    : JOneBotEvent(Time, SelfId, PostType);
internal record JOneBotEvent_Request(long Time, string? SelfId, string? PostType)
    : JOneBotEvent(Time, SelfId, PostType);
internal record JOneBotEvent_MetaEvent(long Time, string? SelfId, string? PostType)
    : JOneBotEvent(Time, SelfId, PostType);
