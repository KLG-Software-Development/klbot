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
        bool FileExist(string relative_path);
        void SaveFileAsString(string relative_path, string text);
        void SaveFileAsBinary(string relative_path, byte[] bin);
        string ReadFileAsString(string relative_path);
        byte[] ReadFileAsBinary(string relative_path);
        void DeleteFile(string relative_path);
    }
    /// <summary>
    /// 发送消息API
    /// </summary>
    public interface IMessagingAPI
    {
        void SendMessage(MessageContext context, long user_id, long group_id, string content);
        void ReplyMessage(Message origin_msg, string content);
        void SendGroupMessage(long group_id, string content);
        void SendGroupMessage(long user_id, long group_id, string content);
        void SendPrivateMessage(long user_id, string content);
    }
    /// <summary>
    /// 模块访问API
    /// </summary>
    public interface IModuleAccessAPI
    {
        T GetModule<T>(int index = 0) where T : Module;
        bool TryGetFieldAndProperty<T>(string name, out T value);
    }
}
