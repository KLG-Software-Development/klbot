using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Http;
using System.Text;
using System.Threading.Tasks;

namespace klbotlib.MessageServer.Mirai;

internal class MiraiNetworkHelper
{
    private static HttpClient _client = new();

    //返回发送特定上下文消息的url
    internal static string GetSendMessageUrl(string serverUrl, MessageContext context)
    {
        string url;
        if (context == MessageContext.Group)
            url = $"{serverUrl}/sendGroupMessage";
        else if (context == MessageContext.Private)
            url = $"{serverUrl}/sendFriendMessage";
        else if (context == MessageContext.Temp)
            url = $"{serverUrl}/sendTempMessage";
        else
            throw new Exception($"暂不支持的消息上下文类型 \"{context}\"");
        return url;
    }
    internal static string GetUploadFileUrl(string serverUrl, MessageContext context)
        => serverUrl + "/file/upload";

    //返回获取消息的url
    internal static string GetFetchMessageUrl(string serverUrl)
        => $"{serverUrl}/fetchMessage";
    internal static string FetchMessageListJSON(string serverUrl, string encoding = "utf-8")
    {
        _client.DefaultRequestHeaders.AcceptEncoding.Clear();
        _client.DefaultRequestHeaders.AcceptEncoding.TryParseAdd(encoding);
        return _client.GetStringAsync(GetFetchMessageUrl(serverUrl)).Result;
    }
}
