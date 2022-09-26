using klbotlib.Exceptions;
using klbotlib.Internal;
using klbotlib.MessageServer.Mirai.JsonPrototypes;
using klbotlib.Modules;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;

namespace klbotlib.MessageServer.Mirai;

/// <summary>
/// MessageServer的Mirai兼容实现
/// </summary>
public class MiraiMessageServer : IMessageServer
{
    private Task<Exception> _networkTask;   //网络相关任务队列

    /// <summary>
    /// Mirai服务器URL
    /// </summary>
    public string ServerURL { get; }
    /// <summary>
    /// 创建一个Mirai消息服务器
    /// </summary>
    public MiraiMessageServer(string serverUrl)
    {
        ServerURL = serverUrl;
        _networkTask = Task.Run(() => (Exception)null);
    }

    /// <summary>
    /// 返回未读消息列表
    /// </summary>
    /// <returns></returns>
    public List<Message> FetchMessages()
    {
        List<Message> msgs = new();
        JMiraiFetchMessageResponse obj = null;
        do
        {
            string response = MiraiNetworkHelper.FetchMessageListJSON(ServerURL);
            try
            {
                //构建直接JSON对象
                obj = JsonConvert.DeserializeObject<JMiraiFetchMessageResponse>(response);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"JSON解析失败：{ex.Message}");
                File.AppendAllText("errorMsg.log", $"[{DateTime.Now:G}]\n{response}");
                Console.WriteLine($"错误源JSON字符串已记录至“errorMsg.json”");
                Console.Write("> ");
                continue;
            }
            List<JMiraiMessagePackage> jmsgs = obj.data.ToList();
            foreach (JMiraiMessagePackage jmsg in jmsgs)
            {
                try
                {
                    msgs.Add(MiraiMessageFactory.BuildMessage(jmsg));
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"Message对象构造失败：{ex.Message}");
                    File.AppendAllText("errorMsg.log", $"[{DateTime.Now:G}]\n{response}");
                    Console.WriteLine($"错误源JSON字符串已记录至“errorMsg.json”");
                    Console.Write("> ");
                    continue;
                }
            }
        }
        while (obj != null && obj.data.Count != 0);   //无限轮询直到拿下所有消息
        return msgs.Where(x => !(x is MessageEmpty)).ToList(); //预过滤空消息
    }
    /// <inheritdoc/>
    public Message GetMessageFromID(long id)
    {
        string response = MiraiNetworkHelper.GetMessageByIdJSON(ServerURL, id);
        JMiraiGetMessageFromIdResponse obj = null;
        try
        {
            obj = JsonConvert.DeserializeObject<JMiraiGetMessageFromIdResponse>(response);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"JSON解析失败：{ex.Message}");
            File.AppendAllText("errorMsg.log", $"[{DateTime.Now:G}]\n{response}");
            Console.WriteLine($"错误源JSON字符串已记录至“errorMsg.json”");
        }
        try
        {
            return MiraiMessageFactory.BuildMessage(obj.data);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Message对象构造失败：{ex.Message}");
            File.AppendAllText("errorMsg.log", $"[{DateTime.Now:G}]\n{response}");
            Console.WriteLine($"错误源JSON字符串已记录至“errorMsg.json”");
            throw ex;
        }
    }
    /// <summary>
    /// 发送消息
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="context">上下文</param>
    /// <param name="userId">目标用户ID</param>
    /// <param name="groupId">目标群组ID</param>
    /// <param name="content">MsgMarker内容</param>
    /// <returns>发送消息过程中的异常。如果一切正常返回值为null</returns>
    public Exception SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
    {
        Exception exception = null;
        //编译MsgMarker文本到json消息链
        string chainJson;
        try
        {
            chainJson = MiraiMsgMarkerTranslater.CompileMessageChainJson(content);
        }
        catch (Exception ex)
        {
            exception = ex;
            chainJson = MiraiJsonHelper.MiraiMessageElementBuilder.BuildPlainElement($"{module.ModuleID}返回的MsgMarker文本不符合语法。异常信息：\n{ex.GetType().Name}：{ex.Message}\n\n调用栈：\n{ex.StackTrace}");
        }
        //创建完整JSON字符串
        string fullJson = MiraiJsonHelper.MiraiMessageJsonBuilder.BuildMessageJson(userId, groupId, context, chainJson);
        //上一条消息已完成，则直接用新任务覆盖
        if (_networkTask.IsCompleted)
        {
            _networkTask = Task.Run(() => TrySendMessage(context, fullJson));
            _networkTask.ContinueWith(x => CheckNetworkTaskResult(x.Result, module, userId, groupId, context));
        }
        //上一条消息未完成，排队
        else
        {
            _networkTask.ContinueWith((x) =>
            {
                CheckNetworkTaskResult(x.Result, module, userId, groupId, context);   //检查上一条消息的完成结果 若有问题打印相关信息
                TrySendMessage(context, fullJson);  //尝试发送下一条消息
            });
        }
        return exception;
    }
    /// <summary>
    /// 上传群文件
    /// </summary>
    /// <param name="module">模块</param>
    /// <param name="groupId">目标群组ID</param>
    /// <param name="uploadPath">上传路径</param>
    /// <param name="filePath">文件本地路径</param>
    /// <returns>发送消息过程中的异常。如果一切正常返回值为null</returns>
    [Obsolete("该方法仍有问题")]
    public Exception UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        throw new NotImplementedException();
#pragma warning disable CS0162 // 检测到无法访问的代码
        Exception exception = null;
        try
        {
            var multipart = new MultipartFormDataContent();
            multipart.Add(new StringContent("group"), "type");
            multipart.Add(new StringContent(groupId.ToString()), "target");
            multipart.Add(new StringContent(uploadPath), "path");
            FileStream fs = new FileStream(filePath, FileMode.Open);
            multipart.Add(new StreamContent(fs), "file");
            //发送
            if (_networkTask.IsCompleted)
            {
                _networkTask = Task.Run(() =>
                {
                    Exception ex = TryUploadFile(MessageContext.Group, fs, multipart);
                    CheckNetworkTaskResult(ex, module, groupId, groupId, MessageContext.Group);
                    return ex;
                });
            }
            else
            {
                _networkTask.ContinueWith((x) =>
                {
                    TryUploadFile(MessageContext.Group, fs, multipart);  //尝试上传消息
                    CheckNetworkTaskResult(x.Result, module, groupId, groupId, MessageContext.Group);   //检查上一个任务的完成结果 若有问题打印相关信息
                });
            }
        }
        catch (Exception ex)
        {
            return ex;
        }
        return exception;
#pragma warning restore CS0162 // 检测到无法访问的代码
    }

    // 发送给定消息.
    private Exception TrySendMessage(MessageContext context, string fullMsgJson)
    {
        string url = MiraiNetworkHelper.GetSendMessageUrl(ServerURL, context);
        try
        {
            bool result = GeneralNetworkHelper.PostPlainText(url, fullMsgJson, out string responseStr);
            if (!result)
                throw new Exception($"非成功返回码：{responseStr}");
            var response = JsonConvert.DeserializeObject<JMiraiSendMessageResponse>(responseStr);
            if (response.code != 0)
                throw new MiraiException(response.code, response.msg);
            return null;
        }
        catch (Exception ex)
        {
            return ex;
        }
    }
    // 发送给定消息.
    private Exception TryUploadFile(MessageContext context, FileStream fs, MultipartFormDataContent fullContent)
    {
        string url = MiraiNetworkHelper.GetUploadFileUrl(ServerURL, context);
        try
        {
            bool result = GeneralNetworkHelper.PostMultipart(url, fullContent, out string responseStr);
            if (!result)
                throw new Exception($"非成功返回码：{responseStr}");
            var response = JsonConvert.DeserializeObject<JMiraiSendMessageResponse>(responseStr);
            if (response.code != 0)
                throw new MiraiException(response.code, response.msg);
            fs.Close();
            return null;
        }
        catch (Exception ex)    //错误会被记录在DiagData中
        {
            fs.Close();
            return ex;
        }
    }
    // 检查上一个发送任务是否正确完成，若失败则根据异常决定是否尝试使机器人发送错误消息
    private void CheckNetworkTaskResult(Exception exception, Module module, long userId, long groupId, MessageContext context)
    {
        if (exception != null)   //检查上个发送任务是否正确完成
        {
            if (exception is MiraiException) //极可能是消息本身的问题，可以尝试发送错误信息
            {
                string chainJson = MiraiJsonHelper.MiraiMessageElementBuilder.BuildPlainElement($"{module.ModuleID}返回的消息不受mirai服务器认可。\n异常信息：\n{exception.Message}");
                string fullJson = MiraiJsonHelper.MiraiMessageJsonBuilder.BuildMessageJson(userId, groupId, context, chainJson);
                TrySendMessage(context, fullJson);
            }
        }
    }
    /// <inheritdoc/>
    public bool Verify(string key)
    {
        try
        {
            JMiraiResponse response = JsonConvert.DeserializeObject<JMiraiResponse>(MiraiNetworkHelper.Verify(ServerURL, key));
            response.CheckStatusCode();
            return true;
        }
        catch
        {
            return false;
            throw;
        }
    }
}
