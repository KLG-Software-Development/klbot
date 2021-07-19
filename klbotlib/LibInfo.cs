using System;
using System.Reflection;

namespace klbotlib.Info
{
    public static class LibInfo
    {
        public static Version GetLibVersion() => Assembly.GetExecutingAssembly().GetName().Version;
    }
}
