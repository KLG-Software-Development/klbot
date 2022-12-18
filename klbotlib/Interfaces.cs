using klbotlib.Modules;
using System;
using System.Threading.Tasks;

namespace klbotlib
{
    /// <summary>
    /// 文件操作API
    /// </summary>
    public interface IFileAPI
    {
        /// <summary>
        /// 返回模块缓存目录中是否存在某个文件
        /// </summary>
        /// <param name="relativePath">对模块缓存目录的相对路径</param>
        bool FileExist(string relativePath);
        /// <summary>
        /// 保存文本到模块缓存目录
        /// </summary>
        /// <param name="relativePath">对模块缓存目录的相对路径</param>
        /// <param name="text">保存的内容</param>
        void SaveFileAsString(string relativePath, string text);
        /// <summary>
        /// 保存二进制到模块缓存目录
        /// </summary>
        /// <param name="relativePath">对模块缓存目录的相对路径</param>
        /// <param name="bin">保存的内容</param>
        void SaveFileAsBinary(string relativePath, byte[] bin);
        /// <summary>
        /// 从模块缓存目录里读取文本
        /// </summary>
        /// <param name="relativePath">要读取的文件对模块缓存目录的相对路径</param>
        string ReadFileAsString(string relativePath);
        /// <summary>
        /// 从模块缓存目录里按行读取文本到字符串数组
        /// </summary>
        /// <param name="relativePath">要读取的文件对模块缓存目录的相对路径</param>
        string[] ReadFileAsStringArrayByLines(string relativePath);
        /// <summary>
        /// 从模块缓存目录里读取二进制
        /// </summary>
        /// <param name="relativePath">要读取的文件对模块缓存目录的相对路径</param>
        byte[] ReadFileAsBinary(string relativePath);
        /// <summary>
        /// 从模块缓存目录里删除文件
        /// </summary>
        /// <param name="relativePath">要删除的文件对模块缓存目录的相对路径</param>
        void DeleteFile(string relativePath);
    }
    /// <summary>
    /// 消息API
    /// </summary>
    public interface IMessagingAPI
    {
        /// <summary>
        /// 发送消息接口
        /// </summary>
        /// <param name="context">发送的消息上下文类型</param>
        /// <param name="userId">用户ID</param>
        /// <param name="groupId">群组ID</param>
        /// <param name="content">MsgMarker文本</param>
        Task SendMessage(MessageContext context, long userId, long groupId, string content);
        /// <summary>
        /// 回复消息接口
        /// </summary>
        /// <param name="originMsg">待回复的原始消息</param>
        /// <param name="content">回复内容</param>
        Task ReplyMessage(MessageCommon originMsg, string content);
        /// <summary>
        /// 发送群消息接口
        /// </summary>
        /// <param name="groupId">目标群组ID</param>
        /// <param name="content">MsgMarker文本</param>
        Task SendGroupMessage(long groupId, string content);
        /// <summary>
        /// 发送临时消息接口
        /// </summary>
        /// <param name="userId">目标用户ID</param>
        /// <param name="groupId">通过的群组的ID</param>
        /// <param name="content">MsgMarker文本</param>
        Task SendTempMessage(long userId, long groupId, string content);
        /// <summary>
        /// 发送私聊消息接口
        /// </summary>
        /// <param name="userId">目标用户ID</param>
        /// <param name="content">MsgMarker文本</param>
        Task SendPrivateMessage(long userId, string content);
        /// <summary>
        /// 上传文件接口
        /// </summary>
        /// <param name="context">上传文件的上下文</param>
        /// <param name="target">目标ID</param>
        /// <param name="uploadPath">上传路径</param>
        /// <param name="filePath">文件本地路径</param>
        Task UploadFile(MessageContext context, long target, string uploadPath, string filePath);
        /// <summary>
        /// 根据消息ID获取消息接口
        /// </summary>
        /// <param name="target">获取的目标ID</param>
        /// <param name="messageId">消息ID</param>
        /// <returns>消息对象</returns>
        Task<Message> GetMessageFromID(long target, long messageId);
    }
    /// <summary>
    /// 模块访问API
    /// </summary>
    public interface IModuleAccessAPI
    {
        /// <summary>
        /// 获取宿主KLBot上其他模块的标准方法。根据模块类型和索引，从宿主KLBot处获取模块实例
        /// </summary>
        /// <typeparam name="T">目标模块的类型</typeparam>
        /// <param name="index">目标模块在同类型模块中的索引。默认为0</param>
        /// <returns>获取到的模块实例</returns>
        T GetModule<T>(int index = 0) where T : Module;
        /// <summary>
        /// 尝试获取模块特定字段的值。只允许获取public字段
        /// </summary>
        /// <typeparam name="T">字段类型</typeparam>
        /// <param name="name">字段名称</param>
        /// <param name="value">输出字段的值</param>
        /// <returns>是否获取成功</returns>
        bool TryGetFieldAndProperty<T>(string name, out T value) where T : struct;
    }
    /// <summary>
    /// 操作API
    /// </summary>
    public interface IOperationAPI
    {
        /// <summary>
        /// 禁言指定用户
        /// </summary>
        /// <param name="userId">待禁言者ID</param>
        /// <param name="groupId">所在群ID</param>
        /// <param name="durationSeconds">禁言时长</param>
        Task Mute(long userId, long groupId, uint durationSeconds);
        /// <summary>
        /// 解除指定用户的禁言
        /// </summary>
        /// <param name="userId">待解除禁言者ID</param>
        /// <param name="groupId">所在群ID</param>
        Task Unmute(long userId, long groupId);
    }
}