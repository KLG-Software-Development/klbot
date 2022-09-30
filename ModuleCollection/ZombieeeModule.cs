﻿using klbotlib;
using klbotlib.Modules;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net.Http;
using System.Security.Cryptography;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;

namespace ModuleCollection;

/// <summary>
/// 僵尸文学模块
/// </summary>
public class ZombieeeModule : SingleTypeModule<MessagePlain>
{
    private static readonly Dictionary<string, string> _langCode = new()
    {
        { "中文", "zh" },
        { "英文", "en" },
        { "韩语", "kor" },
        { "日语", "jp" },
        { "世界语", "epo" }
    };
    private static readonly Dictionary<string, string> _errorMsg = new()
    {
        { "52000", "成功" },
        { "52001", "请求超时" },
        { "52002", "系统错误" },
        { "52003", "未授权用户" },
        { "54000", "必填参数为空" },
        { "54001", "签名错误" },
        { "54003", "访问频率受限" },
        { "54005", "长query请求频繁" },
    };
    private static readonly char[] _marks = new[] { '。', '。', '。', '？', '？', '，', '，', '，', '，', '，', '！', '！', '…' };
    private static readonly HashSet<string> _typeNames = new()
    {
        "N",
        "V",
        "ADJ",
        "ADV",
        "CLAS",
        "ECHO",
        "STRU",
        "AUX",
        "COOR",
        "CONJ",
        "SUFFIX",
        "PREFIX",
        "PREP",
        "PRON",
        "QUES",
        "NUM",
        "IDIOM"
    };
    private readonly StringBuilder _sb = new();
    private static readonly Random _ro = new();
    private static readonly MD5 _md5 = MD5.Create();
    private readonly HttpClient _client = new();
    private readonly Regex _dstPat = new(@""",""dst"":""(.*)""}]}$", RegexOptions.Compiled);
    private readonly Regex _errorCodePat = new(@"""error_code"":""(\d+)""", RegexOptions.Compiled);
    private readonly Dictionary<int, double> _generalizedHarmonicNumber = new();

    [ModuleSetup]
    private string _baseUrl = string.Empty;
    [ModuleSetup]
    private string _appID = string.Empty; //APP ID
    [ModuleSetup]
    private string _key = string.Empty;   //APP KEY
    [ModuleStatus]
    private string _srcLang = string.Empty;       //源语言
    [ModuleStatus]
    private string _dstLang = string.Empty;         //目标语言
    [ModuleSetup]
    private double _power = -0.5;     //幂
    [ModuleSetup]
    private int _reflectNum = 3;     //反射次数
    [ModuleSetup]
    private readonly List<string>[] _patterns = new List<string>[0];  //句式模板
    [ModuleSetup]
    private readonly Dictionary<string, string[]> _dicOf = new();   //词性字典

    /// <inheritdoc/>
    public override string FriendlyName => "僵尸文学模块";
    /// <inheritdoc/>
    public override string HelpInfo => "使用方法：@机器人并发送“生成僵尸文学”";

    /// <inheritdoc/>
    public override string? Filter(MessagePlain msg)
    {
        string text = msg.Text.Trim();
        if (msg.TargetID.Contains(HostBot.SelfID) && text == "生成僵尸文学")
            return "generate";
        else
            return null;
    }
    /// <inheritdoc/>
    public override string? Processor(MessagePlain msg, string? filterOut)
    {
        switch (filterOut)
        {
            case "generate":
                return GenerateZombieeeText();
            default:
                return $"意外遭遇未知过滤器输出“{filterOut}”。检查模块实现";
        }
    }

    /// <summary>
    /// 生成一段僵尸文学
    /// </summary>
    /// <returns>僵尸文学文本</returns>
    public string GenerateZombieeeText()
    {
        if (!TryGenerate(out string message, out string? result) || result == null)
            return $"生成失败：{message}";
        else
            return result;
    }
    /// <summary>
    /// 尝试用当前设定的源语言和目标语言翻译指定内容
    /// </summary>
    /// <param name="query">待翻译内容</param>
    /// <param name="message">错误信息</param>
    /// <param name="result">翻译结果。若翻译失败内容为null</param>
    /// <returns>翻译是否成功</returns>
    public bool TryTranslate(string query, out string message, out string? result)
        => TryTranslate(query, _srcLang, _dstLang, out message, out result);
    /// <summary>
    /// 尝试用给定的源语言和当前设定的目标语言翻译指定内容
    /// </summary>
    /// <param name="query">待翻译内容</param>
    /// <param name="srcLang">源语言</param>
    /// <param name="message">错误信息</param>
    /// <param name="result">翻译结果。若翻译失败内容为null</param>
    /// <returns>翻译是否成功</returns>
    public bool TryTranslate(string query, string srcLang, out string message, out string? result)
        => TryTranslate(query, srcLang, _dstLang, out message, out result);
    /// <summary>
    /// 尝试生成一段僵尸文学
    /// </summary>
    /// <param name="message">错误信息</param>
    /// <param name="result">生成结果</param>
    /// <returns>生成是否成功</returns>
    public bool TryGenerate(out string message, out string? result)
    {
        string seed = GenerateSentences(_ro.Next(1, 7));
        ModulePrint($"反射种子：{seed}");
        return TryReflect(_reflectNum, seed, out message, out result);
    }
    /// <summary>
    /// 尝试快速生成一短句僵尸文学。
    /// 此方法中句子长度锁定为1，反射次数锁定为2，以保证速度。
    /// </summary>
    /// <param name="message">错误信息</param>
    /// <param name="result">生成结果</param>
    /// <returns>生成是否成功</returns>
    public bool TryFastGenerate(out string message, out string? result)
    {
        string seed = GenerateSingleSentence();
        ModulePrint($"反射种子：{seed}");
        return TryReflect(2, seed, out message, out result);
    }

    private string GetRequestUrl(string query, string srcLang, string dstLang, string salt, string sign)
        => $"{_baseUrl}?q={Uri.EscapeDataString(query)}&from={srcLang}&to={dstLang}&appid={_appID}&salt={salt}&sign={sign}";
    private bool TryTranslate(string query, string srcLang, string dstLang, out string message, out string? result)
    {
        try
        {
            result = null;
            string salt = _ro.Next().ToString();
            string sign = CalculateSign(query, salt);
            string url = GetRequestUrl(query, srcLang, dstLang, salt, sign);
            var response = _client.GetAsync(url).Result;
            if (!response.IsSuccessStatusCode)
            {
                message = $"请求返回异常状态码[{response.StatusCode}]";
                return false;
            }
            else
            {
                string re = response.Content.ReadAsStringAsync().Result;
                response.Dispose();
                if (_dstPat.IsMatch(re))
                {
                    result = Regex.Unescape(_dstPat.Match(re).Groups[1].Value);
                    message = "翻译成功";
                    return true;
                }
                else
                {
                    result = null;
                    if (_errorCodePat.IsMatch(re))
                        message = $"翻译失败：{_errorMsg[_errorCodePat.Match(re).Groups[1].Value]}";
                    else
                        message = "翻译失败：未正确识别到翻译结果";
                    return false;
                }
            }
        }
        catch (Exception ex)
        {
            message = $"翻译时发生异常：{ex.Message}";
            result = null;
            return false;
        }
    }
    private bool TryReflect(string query, out string message, out string? result)
    {
        if (!TryTranslate(query, _srcLang, _dstLang, out message, out string? dst1) || dst1 == null)
        {
            result = null;
            return false;
        }
        Thread.Sleep(1000);
        if (!TryTranslate(dst1, _dstLang, _srcLang, out message, out result))
            return false;
        return true;
    }
    private bool TryReflect(int reflectNum, string query, out string message, out string? result)
    {
        result = query;
        for (int n = 0; n < reflectNum; n++)
        {
            ModulePrint($"正在进行反射#{n}...");
            if (!TryReflect(query, out message, out result))
                return false;
            Thread.Sleep(1000);
        }
        message = "反射成功";
        return true;
    }
    private string GenerateSingleSentence()
    {
        _sb.Clear();
        int patNumber = _ro.Next(_patterns.Length);
        List<string> pattern = _patterns[patNumber];
        int N = pattern.Count;
        for (int n = 0; n < N; n++)
        {
            if (_typeNames.Contains(pattern[n]))
            {
                string typeName = pattern[n];
                int wordIndex = PowerLawDist(_ro.NextDouble(), _power, 0, _dicOf[typeName].Length);
                string word = _dicOf[typeName][wordIndex];
                _sb.Append(word);
            }
            else
                _sb.Append(pattern[n]);
        }
        _sb.Append(_marks[_ro.Next(_marks.Length)]);
        return _sb.ToString();
    }
    private string GenerateSentences(int numSentenceCount)
    {
        _sb.Clear();
        for (int i = 0; i < numSentenceCount; i++)
        {
            int patNumber = _ro.Next(_patterns.Length);
            List<string> pattern = _patterns[patNumber];
            int N = pattern.Count;
            for (int n = 0; n < N; n++)
            {
                if (_typeNames.Contains(pattern[n]))
                {
                    string typeName = pattern[n];
                    int wordIndex = PowerLawDist(_ro.NextDouble(), _power, 0, _dicOf[typeName].Length);
                    string word = _dicOf[typeName][wordIndex];
                    _sb.Append(word);
                }
                else
                    _sb.Append(pattern[n]);
            }
            _sb.Append(_marks[_ro.Next(_marks.Length)]);
        }
        return _sb.ToString();
    }
    private string CalculateSign(string query, string salt)    //签名
    {
        string hashString = $"{_appID}{query}{salt}{_key}";
        byte[] hash = _md5.ComputeHash(Encoding.UTF8.GetBytes(hashString));
        _sb.Clear();
        for (int i = 0; i < hash.Length; i++)
        {
            _sb.Append(Convert.ToString(hash[i], 16).PadLeft(2, '0'));
        }
        return _sb.ToString();
    }
    private int PowerLawDist(double y, double n, int min, int N)
    {
        double ss = 1 - _power;
        int index = Convert.ToInt32(Math.Round(Math.Pow(Hns(N) * ss * _ro.NextDouble(), 1 / ss)));
        return index > N ? N : index;
    }
    //计算generalized harmonic number
    private double Hns(int N)
    {
        if (_generalizedHarmonicNumber.ContainsKey(N))
            return _generalizedHarmonicNumber[N];
        else
        {
            double sum = 0;
            for (int n = 1; n <= N; n++)
            {
                sum += 1.0 / Math.Pow(n, _power);   //Sum 1/(1~N)^_power
            }
            _generalizedHarmonicNumber.Add(N, sum);
            return sum;
        }
    }
}
