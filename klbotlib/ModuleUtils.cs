using System.Net.Http.Json;
using System.Text;

namespace klbotlib.Modules.ModuleUtils;

/// <summary>
/// 为模块准备的Http帮助类
/// </summary>
public class HttpHelper : IKLBotLogUnit
{
    private readonly CancellationToken _cancellationToken = new();
    private static readonly HttpClientHandler s_noProxyHandler = new() { UseProxy = false };

    /// <summary>
    /// 进行所有请求时使用的HttpClient对象
    /// </summary>
    public HttpClient InnerClient { get; }

    /// <summary>
    /// 进行所有请求时的超时时间（秒）。默认为15。
    /// </summary>
    public int Timeout
    {
        get => InnerClient.Timeout.Seconds;
        set => InnerClient.Timeout = new TimeSpan(0, 0, 0, value, 0);
    }
    /// <summary>
    /// 进行所有请求时使用的编码
    /// </summary>
    public string ContentEncoding
    {
        get => InnerClient.DefaultRequestHeaders.AcceptEncoding.First().ToString();
        set
        {
            InnerClient.DefaultRequestHeaders.AcceptEncoding.Clear();
            if (!InnerClient.DefaultRequestHeaders.AcceptEncoding.TryParseAdd(value))
                this.LogError($"警告: 设置编码为\"{value}\"失败。编码未改变");
        }
    }
    /// <inhericdoc/>
    public string LogUnitName => "General/HttpHelper";

    /// <summary>
    /// 创建新的HttpHelper对象
    /// </summary>
    /// <param name="timeout">超时(毫秒)</param>
    /// <param name="contentEncoding">默认Encoding</param>
    /// <param name="useSystemProxy">是否使用系统代理</param>
    /// <param name="ua">请求时使用的UA标识。默认为Firefox</param>
    public HttpHelper(int timeout = 15, string contentEncoding = "utf-8", bool useSystemProxy = false, string ua = "Mozilla/5.0 (Windows NT 6.1; rv:2.0.1) Gecko/20210713 Firefox/90.0")
    {
        InnerClient = useSystemProxy
            ? new()
            : new(s_noProxyHandler);
        Timeout = timeout;
        ContentEncoding = contentEncoding;
        InnerClient.DefaultRequestHeaders.UserAgent.Clear();
        InnerClient.DefaultRequestHeaders.UserAgent.ParseAdd(ua);
    }

    /// <summary>
    /// 向指定地址发送GET请求
    /// </summary>
    /// <param name="url">地址</param>
    public async Task<HttpResponseMessage> GetAsync(string url)
    {
        return await InnerClient.GetAsync(url, _cancellationToken);
    }
    /// <summary>
    /// 从指定地址GET字节数组
    /// </summary>
    /// <param name="url">地址</param>
    public async Task<byte[]> GetBytesAsync(string url)
    {
        return await InnerClient.GetByteArrayAsync(url, _cancellationToken);
    }
    /// <summary>
    /// 从指定地址GET一条字符串
    /// </summary>
    /// <param name="url">地址</param>
    public async Task<string> GetStringAsync(string url)
    {
        return await InnerClient.GetStringAsync(url, _cancellationToken);
    }
    /// <summary>
    /// GET内容并转换为Base64字符串
    /// </summary>
    /// <param name="url">地址</param>
    public async Task<string> GetAsBase64Async(string url)
    {
        return Convert.ToBase64String(await InnerClient.GetByteArrayAsync(url, _cancellationToken));
    }
    /// <summary>
    /// 向指定地址POST一条字符串
    /// </summary>
    /// <param name="url">地址</param>
    /// <param name="body">内容</param>
    /// <param name="mime">MIME类型，默认为JSON</param>
    public async Task<string> PostStringAsync(string url, string body, string mime = "application/json")
    {
        StringContent content = new(body, Encoding.GetEncoding(ContentEncoding), mime);
        return await InnerClient.PostAsync(url, content, _cancellationToken).Result.Content.ReadAsStringAsync();
    }
    /// <summary>
    /// 向指定地址POST一组x-www-form-urlencoded内容
    /// </summary>
    /// <param name="url"></param>
    /// <param name="body"></param>
    /// <returns></returns>
    public async Task<string> PostFormUrlEncodedAsync(string url, string body)
    {
        if (body.StartsWith("?"))
            body = body[1..];
        //解析formUrlEncoded
        List<KeyValuePair<string, string>> form = [];
        string[] kvps = body.Split('&');
        foreach (var kvpString in kvps)
        {
            string[] kvp = kvpString.Split('=');
            if (kvp.Length >= 2)
                form.Add(new KeyValuePair<string, string>(kvp[0], kvp[1]));
            else
                form.Add(new KeyValuePair<string, string>(kvp[0], ""));
        }
        FormUrlEncodedContent content = new(form);
        return await InnerClient.PostAsync(url, content, _cancellationToken).Result.Content.ReadAsStringAsync();
    }
    /// <summary>
    /// 向指定地址POST一段JSON内容
    /// </summary>
    /// <typeparam name="T">待序列化对象的类型</typeparam>
    /// <param name="url"></param>
    /// <param name="body">待序列化的对象</param>
    /// <returns></returns>
    public async Task<string> PostJsonAsync<T>(string url, T body)
    {
        return await InnerClient.PostAsJsonAsync(url, body).Result.Content.ReadAsStringAsync();
    }
}
