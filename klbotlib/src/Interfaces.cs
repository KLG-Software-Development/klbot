using klbotlib.Modules;
using System;
using System.Collections.Generic;
using System.Text;

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
        /// <param name="relative_path">对模块缓存目录的相对路径</param>
        bool FileExist(string relative_path);
        /// <summary>
        /// 保存文本到模块缓存目录
        /// </summary>
        /// <param name="relative_path">对模块缓存目录的相对路径</param>
        /// <param name="text">保存的内容</param>
        void SaveFileAsString(string relative_path, string text);
        /// <summary>
        /// 保存二进制到模块缓存目录
        /// </summary>
        /// <param name="relative_path">对模块缓存目录的相对路径</param>
        /// <param name="bin">保存的内容</param>
        void SaveFileAsBinary(string relative_path, byte[] bin);
        /// <summary>
        /// 从模块缓存目录里读取文本
        /// </summary>
        /// <param name="relative_path">要读取的文件对模块缓存目录的相对路径</param>
        string ReadFileAsString(string relative_path);
        /// <summary>
        /// 从模块缓存目录里读取二进制
        /// </summary>
        /// <param name="relative_path">要读取的文件对模块缓存目录的相对路径</param>
        byte[] ReadFileAsBinary(string relative_path);
        /// <summary>
        /// 从模块缓存目录里删除文件
        /// </summary>
        /// <param name="relative_path">要删除的文件对模块缓存目录的相对路径</param>
        void DeleteFile(string relative_path);
    }
    /// <summary>
    /// 发送消息API
    /// </summary>
    public interface IMessagingAPI
    {
        /// <summary>
        /// 发送消息接口
        /// </summary>
        /// <param name="context">发送的消息上下文类型</param>
        /// <param name="user_id">用户ID</param>
        /// <param name="group_id">群组ID</param>
        /// <param name="content">MsgMarker文本</param>
        void SendMessage(MessageContext context, long user_id, long group_id, string content);
        /// <summary>
        /// 回复消息接口
        /// </summary>
        /// <param name="origin_msg">待回复的原始消息</param>
        /// <param name="content">回复内容</param>
        void ReplyMessage(Message origin_msg, string content);
        /// <summary>
        /// 发送群消息接口
        /// </summary>
        /// <param name="group_id">目标群组ID</param>
        /// <param name="content">MsgMarker文本</param>
        void SendGroupMessage(long group_id, string content);
        /// <summary>
        /// 发送临时消息接口
        /// </summary>
        /// <param name="user_id">目标用户ID</param>
        /// <param name="group_id">通过的群组的ID</param>
        /// <param name="content">MsgMarker文本</param>
        void SendTempMessage(long user_id, long group_id, string content);
        /// <summary>
        /// 发送私聊消息接口
        /// </summary>
        /// <param name="user_id">目标用户ID</param>
        /// <param name="content">MsgMarker文本</param>
        void SendPrivateMessage(long user_id, string content);
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
        bool TryGetFieldAndProperty<T>(string name, out T value);
    }
}
