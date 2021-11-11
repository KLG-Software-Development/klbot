using klbotlib;
using klbotlib.Extensions;
using klbotlib.Modules;
using klbotlib.Modules.ModuleUtils;
using System;
using System.Linq;
using System.Text.RegularExpressions;
using System.Xml;

namespace ModuleCollection
{
    public class CollapseModule : SingleTypeModule<MessagePlain>
    {
        private readonly Regex _collapsePat = new Regex(@"塌\s+(.+)");
        private readonly Regex _stepPat = new Regex(@"过程\s+(.+)");
        private readonly HttpHelper _helper = new HttpHelper();
        private readonly XmlDocument _xmlLoader = new XmlDocument();

        public CollapseModule()
        {
            _helper.ContentType = "application/x-www-form-urlencoded";
            _helper.UA = "Wolfram Android App/1.3.0.5403760";
            _helper.Headers.Add("Cookie", "WR_SID=fb447e9e.5cd2d52be64ed");
            _helper.Headers.Add("Cookie2", "$Version=1");
        }

        public override string FriendlyName => "塌塌模块";
        public override bool IsAsync => true;
        public override bool UseSignature => false;
        public override string HelpInfo => "@机器人并发送\"塌 [问题]\"，可直接获取结果；\n发送\"过程 [问题]\"，可获取可能的计算步骤。";
        public override string Filter(MessagePlain msg)
        {
            if (!msg.TargetID.Contains(HostBot.SelfID))
                return null;
            string text = msg.Text.Trim();
            return _collapsePat.IsMatch(text)
                ? "答案"
                : _stepPat.IsMatch(text)
                    ? "过程"
                    : null;
        }
        public override string Processor(MessagePlain msg, string filter_out)
        {
            switch (filter_out)
            {
                case "答案":
                    string input = _collapsePat.Match(msg.Text.Trim()).Groups[1].Value;
                    string xml = _helper.GetString(GetResultUrl(input));
                    return ProcessXML(msg, xml);
                case "过程":
                    input = _stepPat.Match(msg.Text.Trim()).Groups[1].Value;
                    xml = _helper.GetString(GetResultUrl(input));
                    return ProcessXMLStepByStep(msg, xml, input);
            }
            return null;
        }

        private static string GetStepByStepUrl(string input, string pod_state)
            => $"https://api.wolframalpha.com/v2/query.jsp?appid=6682H9-A87PYX7R9A&input={Uri.EscapeDataString(input)}&podstate={Uri.EscapeDataString(pod_state)}&format=image";
        private static string GetResultUrl(string input)
            => $"https://api.wolframalpha.com/v2/query.jsp?appid=6682H9-A87PYX7R9A&input={Uri.EscapeDataString(input)}&format=image";

        private bool TryGetResultRoot(string xml, out XmlNode queryresult)
        {
            _xmlLoader.LoadXml(xml);
            queryresult = _xmlLoader.GetElementsByTagName("queryresult")[0];
            //查询失败
            return queryresult.Attributes["success"].Value == "true";
        }
        //尝试获取primary pod
        private static bool TryGetPrimaryPod(XmlNodeList childs, out XmlNode output)
        {
            XmlNode first_pod = null;
            output = null;
            //获取primary pod
            foreach (XmlNode child in childs)
            {
                if (child.Attributes == null || child.Attributes["primary"] == null || child.Name != "pod")
                    continue;
                else if (child.Attributes["primary"].Value == "true")
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
        //XML解析：答案
        private string ProcessXML(Message msg, string xml)
        {
            if (!TryGetResultRoot(xml, out XmlNode queryresult))
                return "塌了！查询失败，内容可能不合法";
            //查询成功 
            var childs = queryresult.ChildNodes;
            if (!TryGetPrimaryPod(childs, out XmlNode results_pod))
                return "Wolfram Alpha未提供主结果，无法计算";
            //返回图片
            var subpod = results_pod["subpod"];
            if (subpod == null)
                return "塌了！结果无法正确表示";
            var img = subpod["img"];
            if (img == null)
                return "塌了！结果无法正确表示";
            string result_img_url = img.Attributes["src"].Value;
            Messaging.ReplyMessage(msg, $@"\image:\url:{result_img_url}");
            return "没塌！";
        }
        //XML解析：中间过程
        private string ProcessXMLStepByStep(Message msg, string xml, string input)
        {
            if (!TryGetResultRoot(xml, out XmlNode queryresult))
                return "塌了！查询失败，内容可能不合法";
            //查询成功 
            var childs = queryresult.ChildNodes;
            if (!TryGetPrimaryPod(childs, out XmlNode primary_pod))
                return "Wolfram Alpha未提供主结果，无法计算";
            string result_img_url = primary_pod.ChildNodes[1].ChildNodes[1].Attributes["src"].Value;
            Messaging.ReplyMessage(msg, $@"\image:\url:{result_img_url}");
            //检查primary result是否可显示过程
            var states = primary_pod["states"];
            string pod_state = null;
            if (states != null && states.Attributes["count"].Value != "0")
            {
                foreach (XmlNode state in states.ChildNodes)
                {
                    if (state.Attributes != null && state.Attributes["name"].Value == "Step-by-step solution")
                    {
                        pod_state = state.Attributes["input"].Value;
                        break;
                    }
                }
            }
            //第二次查询
            if (pod_state != null)
            {
                xml = _helper.GetString(GetStepByStepUrl(input, pod_state));
                if (!TryGetResultRoot(xml, out queryresult))
                    return "塌了！查询失败，内容可能不合法";
                //查询成功 
                childs = queryresult.ChildNodes;
                if (!TryGetPrimaryPod(childs, out primary_pod))
                    return "Wolfram Alpha未提供主结果，无法计算";
                if (!primary_pod.TryGetFirstChildNodeByAttribute("title", "Possible intermediate steps", out XmlNode step_pod))
                    return "无法显示过程：不存在可用的中间过程";
                result_img_url = step_pod["img"].Attributes["src"].Value;
                Messaging.ReplyMessage(msg, $@"\image:\url:{result_img_url}");
            }
            else
                return "无法显示过程：不存在可用的中间过程";
            return "没塌！";
        }
    }
}
