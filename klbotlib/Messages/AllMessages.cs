using System.Collections.Generic;

namespace klbotlib;

// 忽略的Message类。显然其内部所有不需要处理的JMessage直接对象都会被构建成这个类型。bot在后续处理中会把这种类型的消息全部忽略
internal record MessageEmpty : Message;
/// <summary>
/// 纯文本消息类
/// </summary>
public record MessagePlain(string Text) : Message;
/// <summary>
/// @消息
/// </summary>
public record MessageAt(long TargetId) : Message;
/// <summary>
/// 表情消息
/// </summary>
public record MessageFace(string FaceId) : Message;
/// <summary>
/// 撤回消息
/// </summary>
/// <param name="AuthorId">消息作者ID</param>
/// <param name="MessageId">消息ID</param>
public record MessageRecall(long AuthorId, long MessageId) : Message;
/// <summary>
/// 禁言/解除禁言消息
/// </summary>
/// <param name="Unmute">是否为解除禁言</param>
/// <param name="MemberId">目标群成员ID</param>
/// <param name="DurationSeconds">持续时间。若为解除禁言则此字段无意义</param>
public record MessageMute(bool Unmute, long MemberId, uint DurationSeconds = 0) : Message;
/// <summary>
/// 语音消息
/// </summary>
/// <param name="Url">语音的URL</param>
public record MessageVoice(string Url) : Message;

/// <summary>
/// 图像消息类
/// </summary>
public record MessageImage(string Url, bool IsFlashImage) : Message;
