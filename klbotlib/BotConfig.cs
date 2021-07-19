using System.Collections.Generic;
using System.Linq;

namespace klbotlib
{
    //Bot配置信息
    public class BotConfig
    {
        internal QQ QQ;
        internal Network Network;
        internal SavePathes SavePathes;

        public BotConfig(string url, long self_id, IEnumerable<long> targets, string modules_home_dir)
        {
            QQ = new QQ(self_id, targets);
            Network = new Network(url);
            SavePathes = new SavePathes(modules_home_dir);
        }
    }
    internal class QQ
    {
        //Bot账号的自身ID
        public long SelfID;
        //Bot监听的群组的ID列表
        public List<long> TargetGroupIDList;
        public QQ(long selfID, IEnumerable<long> target_group_list)
        {
            SelfID = selfID;
            TargetGroupIDList = target_group_list.ToList();
        }
    }
    internal class Network
    {
        //mirai服务器的URL
        public string ServerURL;
        public Network(string server)
        {
            ServerURL = server;
        }
    }
    internal class SavePathes
    {
        public string ModulesHomeDir { get; }
        public SavePathes(string modules_home_dir)
        {
            ModulesHomeDir = modules_home_dir;
        }
    }
}
