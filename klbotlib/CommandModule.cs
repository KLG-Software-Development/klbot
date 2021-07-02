using klbotlib.Modules.Commands;
using Newtonsoft.Json;
using System.Collections.Generic;
using System.IO;
using System.Text.RegularExpressions;

namespace klbotlib.Modules
{
    //命令模块
    public class CommandModule : Module<MessagePlain>
    {
        const string prefix = "##";
        private Regex cmdPat = new Regex($@"^{prefix}(.+)$");
        private KLBot hostBot;

        Dictionary<long, AuthorType> Users;
        public List<Command> Cmds = new List<Command>();
        public AuthorType GetAuthorType(long id)
        {
            if (!Users.ContainsKey(id))
                return AuthorType.野人;
            else
                return Users[id];
        }

        public CommandModule(KLBot klbot, params Command[] cmds)
        {
            if (File.Exists("users"))
                Users = JsonConvert.DeserializeObject<Dictionary<long, AuthorType>>(File.ReadAllText("users"));
            else
                Users = new Dictionary<long, AuthorType> { { 365802825, AuthorType.管理员} };
            hostBot = klbot;
            Cmds.AddRange(cmds);
        }

        public override string Processor(MessagePlain msg_plain)
        {
            string cmd_str = cmdPat.Match(msg_plain.Text).Groups[1].Value.ToLower();
            //遍历命令模块中的命令列表，寻找第一个匹配
            foreach (var cmd in Cmds)
            {
                if (cmd.IsCmd(cmd_str))
                    return cmd.Run(hostBot, msg_plain, cmd_str);
            }
            return $"错误：未知命令'{cmd_str}'";
        }
        public override bool Filter(MessagePlain msg) => cmdPat.IsMatch(msg.Text.Trim());
    }

}
