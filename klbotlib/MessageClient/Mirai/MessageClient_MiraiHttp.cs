using klbotlib.Exceptions;
using klbotlib.MessageClient.Mirai.JsonPrototypes;
using klbotlib.Modules;
using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;

namespace klbotlib.MessageClient.Mirai;

/// <summary>
/// IMessageClient的Mirai兼容实现
/// </summary>
public class MiraiMessageClient : IMessageClient
{
    private const string _errorMsgLogPath = "errorMsg.log";
    /// <summary>
    /// Mirai服务器URL
    /// </summary>
    public string ServerURL { get; }
    /// <summary>
    /// 创建一个Mirai消息服务器
    /// </summary>
    public MiraiMessageClient(string serverUrl)
    {
        ServerURL = serverUrl;
    }

    /// <summary>
    /// 返回未读消息列表
    /// </summary>
    /// <returns></returns>
    public async Task<List<Message>> FetchMessages()
    {
        List<Message> msgs = new();
        JMiraiFetchMessageResponse? obj = null;
        do
        {
            string response = await MiraiNetworkHelper.FetchMessageListJSON(ServerURL);
            try
            {
                //构建直接JSON对象
                obj = JsonConvert.DeserializeObject<JMiraiFetchMessageResponse>(response);
                //验证返回码
                if (obj.code != 0)
                    throw new MiraiException(obj.code, obj.msg);
            }
            catch (Exception ex)
            {
                Console.WriteLine($"消息列表获取失败：JSON解析失败。{ex.Message}");
                File.AppendAllText(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
                Console.WriteLine($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
                Console.Write("> ");
                continue;
            }
            if (obj == null || obj.data == null)
                throw new JsonDeserializationException("JSON解析失败。构建的对象为null。服务器响应：", response);
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
                    File.AppendAllText(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
                    Console.WriteLine($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
                    Console.Write("> ");
                    continue;
                }
            }
        }
        while (obj != null && obj.data != null && obj.data.Count != 0);   //无限轮询直到拿下所有消息
        return msgs.Where(x => !(x is MessageEmpty)).ToList(); //预过滤空消息
    }
    /// <inheritdoc/>
    public async Task<Message> GetMessageFromID(long target, long messageId)
    {
        string response = await MiraiNetworkHelper.GetMessageByIdJSON(ServerURL, target, messageId);
        JMiraiGetMessageFromIdResponse? obj = null;
        try
        {
            obj = JsonConvert.DeserializeObject<JMiraiGetMessageFromIdResponse>(response);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"消息ID获取失败：JSON解析失败。{ex.Message}");
            await File.AppendAllTextAsync(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
            Console.WriteLine($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
        }
        try
        {
            if (obj.data == null)
                throw new JsonDeserializationException("解析的对象数据意外为null", response);
            return MiraiMessageFactory.BuildMessage(obj.data);
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Message对象构造失败：{ex.Message}");
            await File.AppendAllTextAsync(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
            Console.WriteLine($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
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
    public async Task SendMessage(Module module, MessageContext context, long userId, long groupId, string content)
    {
        Exception? exception = null;
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
        await MiraiNetworkHelper.TrySendMessage(ServerURL, context, fullJson);
        await CheckNetworkTaskResult(exception, module, userId, groupId, context);
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
    public async Task UploadFile(Module module, long groupId, string uploadPath, string filePath)
    {
        throw new NotImplementedException();
#pragma warning disable CS0162 // 检测到无法访问的代码
        try
        {
            var multipart = new MultipartFormDataContent();
            multipart.Add(new StringContent("group"), "type");
            multipart.Add(new StringContent(groupId.ToString()), "target");
            multipart.Add(new StringContent(uploadPath), "path");
            FileStream fs = new FileStream(filePath, FileMode.Open);
            multipart.Add(new StreamContent(fs), "file");
            Exception? ex = await MiraiNetworkHelper.TryUploadFile(ServerURL, MessageContext.Group, fs, multipart);
            await CheckNetworkTaskResult(ex, module, groupId, groupId, MessageContext.Group);
        }
        catch
        {
            throw;
        }
#pragma warning restore CS0162 // 检测到无法访问的代码
    }
    /// <inheritdoc/>
    public async Task<bool> Verify(string key)
    {
        try
        {
            JMiraiResponse? response = JsonConvert.DeserializeObject<JMiraiResponse>(await MiraiNetworkHelper.Verify(ServerURL, key));
            response.CheckStatusCode();
            return true;
        }
        catch
        {
            return false;
            throw;
        }
    }
    /// <inheritdoc/>
    public async Task Mute(Module module, long userId, long groupId, uint durationSeconds)
    {
        string responseJson = await MiraiNetworkHelper.Mute(ServerURL, userId, groupId, durationSeconds);
        JMiraiResponse? response = JsonConvert.DeserializeObject<JMiraiResponse>(responseJson);
        CheckMiraiResponse(responseJson, response);
    }
    /// <inheritdoc/>
    public async Task Unmute(Module module, long userId, long groupId)
    {
        string responseJson = await MiraiNetworkHelper.Unmute(ServerURL, userId, groupId);
        JMiraiResponse? response = JsonConvert.DeserializeObject<JMiraiResponse>(responseJson);
        CheckMiraiResponse(responseJson, response);
    }
    /// <inheritdoc/>
    public async Task<long> GetSelfID()
    {
        string botListJson = await MiraiNetworkHelper.GetBotListJson(ServerURL);
        JMiraiGetBotListResponse? response = JsonConvert.DeserializeObject<JMiraiGetBotListResponse>(botListJson);
        response.CheckStatusCode();
        if (response.data == null)
            throw new JsonDeserializationException("自身ID获取失败：无法从JSON中解析机器人列表", botListJson);
        else if (response.data.Length == 0)
            throw new Exception("自身ID获取失败：机器人列表为空");
        else if (response.data.Length != 1)
            throw new Exception("自身ID获取失败：机器人不唯一");
        else
            return response.data[0];
    }

    //**** Helper函数 ****
    // 发送给定消息.
    // 发送给定消息.
    // 检查上一个发送任务是否正确完成，若失败则根据异常决定是否尝试使机器人发送错误消息
    private async Task CheckNetworkTaskResult(Exception? exception, Module module, long userId, long groupId, MessageContext context)
    {
        if (exception != null)   //检查上个发送任务是否正确完成
        {
            if (exception is MiraiException) //极可能是消息本身的问题，可以尝试发送错误信息
            {
                string chainJson = MiraiJsonHelper.MiraiMessageElementBuilder.BuildPlainElement($"{module.ModuleID}返回的消息不受mirai服务器认可。\n异常信息：\n{exception.Message}");
                string fullJson = MiraiJsonHelper.MiraiMessageJsonBuilder.BuildMessageJson(userId, groupId, context, chainJson);
                await MiraiNetworkHelper.TrySendMessage(ServerURL, context, fullJson);
            }
        }
    }
    // 检查返回码
    private void CheckMiraiResponse(string originalJson, JMiraiResponse? response)
    {
        if (response == null)
            throw new Exception($"未成功反序列化Mirai服务器返回的JSON消息。\n返回内容：{originalJson}\n");
        //验证返回码
        if (response.code != 0)
            throw new MiraiException(response.code, response.msg);
    }
}
