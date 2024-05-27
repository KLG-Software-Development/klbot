using klbotlib.Events;
using klbotlib.Exceptions;
using klbotlib.MessageDriver.Mirai.JsonPrototypes;
using klbotlib.Modules;
using System;
using System.IO;
using System.Net.Http;
using System.Threading.Tasks;

namespace klbotlib.MessageDriver.Mirai;

/// <summary>
/// IMessageDriver的Mirai兼容实现
/// </summary>
public class MessageDriver_MiraiHttp : IMessageDriver
{
    private const string _errorMsgLogPath = "errorMsg.log";
    /// <summary>
    /// Mirai消息服务器URL
    /// </summary>
    public string ServerUrl { get; }

    /// <inheritdoc/>
    public string DriverInfo => $"Mirai message driver @{ServerUrl}";
    /// <inheritdoc/>
    public string LogUnitName => "Driver/MiraiHttp";

    /// <summary>
    /// 创建一个基于Mirai消息服务的消息驱动器
    /// </summary>
    public MessageDriver_MiraiHttp(string serverUrl)
    {
        ServerUrl = serverUrl;
        _ = FetchMessagesDaemon(); // 开启轮询
    }

    /// <inheritdoc/>
    public event EventHandler<KLBotMessageEventArgs> OnMessageReceived = (_, _) => { };

    /// <inheritdoc/>
    public async Task<Message> GetMessageFromId(long target, long messageId)
    {
        string response = await MiraiNetworkHelper.GetMessageByIdJSON(ServerUrl, target, messageId);
        JMiraiGetMessageFromIdResponse? obj = null;
        try
        {
            obj = MiraiJsonHelper.Deserialize<JMiraiGetMessageFromIdResponse>(response);
        }
        catch (Exception ex)
        {
            this.LogError($"消息ID获取失败：JSON解析失败。{ex.Message}");
            await File.AppendAllTextAsync(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
            this.LogError($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
        }
        try
        {
            if (obj.data == null)
                throw new JsonDeserializationException("解析的对象数据意外为null", response);
            return MiraiMessageFactory.BuildMessage(obj.data);
        }
        catch (Exception ex)
        {
            this.LogError($"Message对象构造失败：{ex.Message}");
            await File.AppendAllTextAsync(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
            this.LogError($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
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
    /// <param name="msg">待发送消息</param>
    /// <returns>发送消息过程中的异常。如果一切正常返回值为null</returns>
    public async Task SendMessage(Module module, MessageContext context, long userId, long groupId, Message msg)
    {
        Exception? exception = null;
        //创建完整JSON字符串
        string fullJson = MiraiJsonHelper.MiraiMessageJsonBuilder.BuildMessageJson(userId, groupId, context, msg);
        await MiraiNetworkHelper.TrySendMessage(ServerUrl, context, fullJson);
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
            Exception? ex = await MiraiNetworkHelper.TryUploadFile(ServerUrl, MessageContext.Group, fs, multipart);
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
            JMiraiResponse? response = MiraiJsonHelper.Deserialize<JMiraiResponse>(await MiraiNetworkHelper.Verify(ServerUrl, key));
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
        string responseJson = await MiraiNetworkHelper.Mute(ServerUrl, userId, groupId, durationSeconds);
        JMiraiResponse? response = MiraiJsonHelper.Deserialize<JMiraiResponse>(responseJson);
        CheckMiraiResponse(responseJson, response);
    }
    /// <inheritdoc/>
    public async Task Unmute(Module module, long userId, long groupId)
    {
        string responseJson = await MiraiNetworkHelper.Unmute(ServerUrl, userId, groupId);
        JMiraiResponse? response = MiraiJsonHelper.Deserialize<JMiraiResponse>(responseJson);
        CheckMiraiResponse(responseJson, response);
    }
    /// <inheritdoc/>
    public async Task<long> GetSelfId()
    {
        string botListJson = await MiraiNetworkHelper.GetBotListJson(ServerUrl);
        JMiraiGetBotListResponse? response = MiraiJsonHelper.Deserialize<JMiraiGetBotListResponse>(botListJson);
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
    // 无限获取消息并触发事件
    private async Task FetchMessagesDaemon()
    {
        this.LogInfo("Fetch messages daemon started.");
        while (true)
        {
            await FetchMessages();
        }
    }
    // 获取消息并逐个触发消息接送事件
    private async Task FetchMessages()
    {
        JMiraiFetchMessageResponse? obj;
        string response = await MiraiNetworkHelper.FetchMessageListJSON(ServerUrl);
        try
        {
            //构建直接JSON对象
            obj = MiraiJsonHelper.Deserialize<JMiraiFetchMessageResponse>(response);
            //验证返回码
            if (obj.code != 0)
                throw new MiraiException(obj.code, obj.msg);
        }
        catch (Exception ex)
        {
            this.LogError($"消息列表获取失败：JSON解析失败。{ex.Message}");
            await File.AppendAllTextAsync(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
            this.LogError($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
            return;
        }
        if (obj == null || obj.data == null)
            throw new JsonDeserializationException("JSON解析失败。构建的对象为null。服务器响应：", response);
        foreach (JMiraiMessagePackage jmsg in obj.data)
        {
            try
            {
                var msg = MiraiMessageFactory.BuildMessage(jmsg);
                OnMessageReceived.Invoke(this, new(DateTime.Now, msg));
            }
            catch (Exception ex)
            {
                this.LogError($"Message对象构造失败：{ex.Message}");
                await File.AppendAllTextAsync(_errorMsgLogPath, $"[{DateTime.Now:G}]\n{response}");
                this.LogError($"错误源JSON字符串已记录至“{_errorMsgLogPath}”");
                continue;
            }
        }
    }
    // 检查上一个发送任务是否正确完成，若失败则根据异常决定是否尝试使机器人发送错误消息
    private async Task CheckNetworkTaskResult(Exception? exception, Module module, long userId, long groupId, MessageContext context)
    {
        if (exception != null)   //检查上个发送任务是否正确完成
        {
            if (exception is MiraiException) //极可能是消息本身的问题，可以尝试发送错误信息
            {
                string fullJson = MiraiJsonHelper.MiraiMessageJsonBuilder.BuildMessageJson(
                    userId, groupId, context,
                    new MessagePlain(userId, groupId, $"{module.ModuleId}返回的消息不受mirai服务器认可。\n异常信息：\n{exception.Message}"));
                await MiraiNetworkHelper.TrySendMessage(ServerUrl, context, fullJson);
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
