﻿using klbotlib.Extensions;
using klbotlib.Modules.ModuleUtils;
using System.Diagnostics.CodeAnalysis;
using System.Text.RegularExpressions;
using System.Xml;

namespace klbotlib.Modules;

/// <summary>
/// 塌塌模块
/// </summary>
public partial class CollapseModule : SingleTypeModule<MessagePackage>
{
    private readonly Regex _collapsePat = CollapsePattern();
    private readonly Regex _stepPat = StepPattern();
    private readonly HttpHelper _helper = new();
    private readonly XmlDocument _xmlLoader = new();

    /// <summary>
    /// 构造函数
    /// </summary>
    public CollapseModule()
    {
        //_helper.UA = "Wolfram Android App/1.3.0.5403760";
        _helper.InnerClient.DefaultRequestHeaders.Add("Cookie", "WR_SID=fb447e9e.5cd2d52be64ed");
        _helper.InnerClient.DefaultRequestHeaders.Add("Cookie2", "$Version=1");
    }

    ///<inheritdoc/>
    public override string FriendlyName => "塌塌模块";
    ///<inheritdoc/>
    public override bool IsAsync => true;
    ///<inheritdoc/>
    public override bool UseSignature => false;
    ///<inheritdoc/>
    public override string HelpInfo => "@机器人并发送\"塌 [问题]\"，可直接获取结果；\n发送\"过程 [问题]\"，可获取可能的计算步骤。";
    ///<inheritdoc/>
    public override async Task<Message?> Processor(MessageContext context, MessagePackage msg)
    {
        if (!msg.TargetIds.Contains(HostBot.SelfId))
            return null;
        string text = msg.AsPlain().Trim();
        if (_collapsePat.IsMatch(text))
        {
            string input = _collapsePat.Match(text).Groups[1].Value;
            string xml = await _helper.GetStringAsync(GetResultUrl(input));
            return await ProcessXml(context, xml);
        }
        else if (_stepPat.IsMatch(text))
        {
            string input = _stepPat.Match(text).Groups[1].Value;
            string xml = await _helper.GetStringAsync(GetResultUrl(input));
            return ProcessXmlStepByStep(context, xml, input);
        }
        return null;
    }

    private static string GetStepByStepUrl(string input, string pod_state)
        => $"https://api.wolframalpha.com/v2/query.jsp?appid=6682H9-A87PYX7R9A&input={Uri.EscapeDataString(input)}&podstate={Uri.EscapeDataString(pod_state)}&format=image";
    private static string GetResultUrl(string input)
        => $"https://api.wolframalpha.com/v2/query.jsp?appid=6682H9-A87PYX7R9A&input={Uri.EscapeDataString(input)}&format=image";

    private bool TryGetResultRoot(string xml, [NotNullWhen(true)] out XmlNode? queryresult)
    {
        _xmlLoader.LoadXml(xml);
        queryresult = _xmlLoader.GetElementsByTagName("queryresult")[0];
        if (queryresult == null)
            return false;
        //查询失败
        var attributes = queryresult.Attributes;
        if (attributes == null)
            return false;
        var success = attributes["success"];
        return success != null && success.Value == "true";
    }
    //尝试获取primary pod
    private static bool TryGetPrimaryPod(XmlNodeList childs, [NotNullWhen(true)] out XmlNode? output)
    {
        XmlNode? first_pod = null;
        output = null;
        //获取primary pod
        foreach (XmlNode child in childs)
        {
            var attributes = child.Attributes;
            if (attributes == null || attributes["primary"] == null || child.Name != "pod")
                continue;
            var primary = attributes["primary"];
            if (primary == null || primary.Value == null)
                continue;
            if (primary.Value == "true")
            {
                if (first_pod != null)
                    first_pod = child;
                output = child;
                break;
            }
        }
        if (output == null)
        {
            //若存在pod 返回第一个
            if (first_pod != null)
            {
                output = first_pod;
                return true;
            }
            //否则是真的没有了
            else
                return false;
        }
        return true;
    }

#pragma warning disable CS8602 // 解引用可能出现空引用。Let it crash

    //XML解析：答案
    private async Task<string> ProcessXml(MessageContext context, string xml)
    {
        if (!TryGetResultRoot(xml, out XmlNode? queryresult))
            return "塌了！查询失败，内容可能不合法";
        //查询成功 
        var childs = queryresult.ChildNodes;
        if (!TryGetPrimaryPod(childs, out XmlNode? resultsPod))
            return "Wolfram Alpha未提供主结果，无法计算";
        //返回图片
        var subpod = resultsPod["subpod"];
        if (subpod == null)
            return "塌了！结果无法正确表示";
        var img = subpod["img"];
        if (img == null)
            return "塌了！结果无法正确表示";
        string result_img_url = img.Attributes["src"].Value;
        await Messaging.ReplyMessage(context, $@"\image:\url:{result_img_url}");
        return "没塌！";
    }

    //XML解析：中间过程
    private string ProcessXmlStepByStep(MessageContext context, string xml, string input)
    {
        if (!TryGetResultRoot(xml, out XmlNode? queryresult))
            return "塌了！查询失败，内容可能不合法";
        //查询成功 
        var childs = queryresult.ChildNodes;
        if (!TryGetPrimaryPod(childs, out XmlNode? primaryPod) || primaryPod == null)
            return "Wolfram Alpha未提供主结果，无法计算";
        string result_img_url = primaryPod.ChildNodes[1].ChildNodes[1].Attributes["src"].Value;
        _ = Messaging.ReplyMessage(context, $@"\image:\url:{result_img_url}");
        //检查primary result是否可显示过程
        var states = primaryPod["states"];
        string? podState = null;
        if (states != null && states.Attributes["count"].Value != "0")
        {
            foreach (XmlNode state in states.ChildNodes)
            {
                if (state.Attributes != null && state.Attributes["name"].Value == "Step-by-step solution")
                {
                    podState = state.Attributes["input"].Value;
                    break;
                }
            }
        }
        //第二次查询
        if (podState != null)
        {
            xml = _helper.GetStringAsync(GetStepByStepUrl(input, podState)).Result;
            if (!TryGetResultRoot(xml, out queryresult))
                return "塌了！查询失败，内容可能不合法";
            //查询成功 
            childs = queryresult.ChildNodes;
            if (!TryGetPrimaryPod(childs, out primaryPod))
                return "Wolfram Alpha未提供主结果，无法计算";
            if (primaryPod == null || !primaryPod.TryGetFirstChildNodeByAttribute("title", "Possible intermediate steps", out XmlNode? step_pod))
                return "无法显示过程：不存在可用的中间过程";
            result_img_url = step_pod["img"].Attributes["src"].Value;
            _ = Messaging.ReplyMessage(context, $@"\image:\url:{result_img_url}");
        }
        else
            return "无法显示过程：不存在可用的中间过程";
        return "没塌！";
    }

#pragma warning restore CS8602 // 解引用可能出现空引用。

    [GeneratedRegex(@"塌\s+(.+)", RegexOptions.Compiled)]
    private static partial Regex CollapsePattern();
    [GeneratedRegex(@"过程\s+(.+)", RegexOptions.Compiled)]
    private static partial Regex StepPattern();
}
