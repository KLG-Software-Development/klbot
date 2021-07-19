using Newtonsoft.Json;
using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text.RegularExpressions;

namespace klbotlib.Modules.CommandModule
{
    //命令模块
    public class CommandModule : SingleTypeModule<MessagePlain>
    {
        const string prefix = "##";
        private readonly Regex cmdPat = new Regex($@"^{prefix}(.+)$");

        [ModuleSetup]
        public string UsersSavePath { get; private set; } = "users";
        [ModuleSetup]
        public Dictionary<long, AuthorType> Users { get; private set; }

        public List<Command> Cmds = new List<Command>();
        public AuthorType GetAuthorType(long id)
        {
            if (!Users.ContainsKey(id))
                return AuthorType.野人;
            else
                return Users[id];
        }

        public CommandModule(KLBot host, params Command[] cmds) : base(host)   //, params Command[] cmds
        {
            if (File.Exists(UsersSavePath))
            {
                HostBot.ModulePrint(this, $"Loading user list from '{UsersSavePath}'...");
                Users = JsonConvert.DeserializeObject<Dictionary<long, AuthorType>>(File.ReadAllText(UsersSavePath));
            }
            else
            {
                HostBot.ModulePrint(this, $"Cannot find users saved file. Loading default...");
                Users = new Dictionary<long, AuthorType> { { 365802825, AuthorType.开发者 } };
            }
            var types = Assembly.GetExecutingAssembly().GetTypes();
            foreach (var type in types)
            {
                if (type.GetRootBaseType() == typeof(Command) && Attribute.GetCustomAttribute(type, typeof(DefaultCommandAttribute)) != null)
                {
                    var constructors = type.GetConstructors();
                    if (constructors.Length > 0)
                        Cmds.Add((Command)constructors[0].Invoke(Array.Empty<object>()));
                }
            }
            int default_cmd_count = Cmds.Count;
            Cmds.AddRange(cmds);
            HostBot.ModulePrint(this, $"Successfully loaded {Cmds.Count} commands ({default_cmd_count} default, {Cmds.Count - default_cmd_count} custom).");
        }

        public override bool IsTransparent => false;
        public override bool Filter(MessagePlain msg) => cmdPat.IsMatch(msg.Text.Trim());
        public override string Processor(MessagePlain msg)
        {
            string cmd_str = cmdPat.Match(msg.Text).Groups[1].Value.ToLower();
            //遍历命令模块中的命令列表，寻找第一个匹配
            foreach (var cmd in Cmds)
            {
                if (cmd.IsCmd(cmd_str))
                    return cmd.Run(HostBot, msg, cmd_str);
            }
            return $"错误：未知命令'{cmd_str}'";
        }
    }

}
