namespace klbotlib.MessageDriver.OneBot.JsonPrototypes;

// 基础的OneBot服务响应
internal record JOneBotResponse<T>(JOneBotSelf? Self, string? Status, long Retcode, T? Data, string? Message, string? Echo);

// OneBotV11事件的数据的抽象设计不佳，因此只能将所有字段的并集堆出
internal record JOneBotEvent(long Time, long SelfId, string? PostType,
    // PostType=message
    string? MessageType, // private/group
    string? SubType,     // private=(friend/group/other); group=(normal/anonymous/notice)
    int MessageId, long GroupId, long UserId, JOneBotMessageContent[]? Message, string? RawMessage // sender字段信息未必准确或齐全且用处不大，省略. 获取发送者信息使用UserId
    // PostType=notice 暂不支持。未来可能会支持的类型：notice_type=group_recall/friend_recall
    // PostType=request 没有任何支持计划
    // PostType=meta_event 没有任何支持计划
);
