namespace klbotlib.MessageDriver.Mirai.JsonPrototypes
{
    internal class JMiraiUser
    {
        public long id;
        public string? memberName;
        public string? permission;
        public JMiraiGroup? group;
        public int GroupId { get => group == null ? -1 : group.id; }
    }
}