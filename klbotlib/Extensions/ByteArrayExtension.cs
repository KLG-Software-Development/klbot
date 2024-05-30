#pragma warning disable CS1591
using System.Linq;
using System.Text;

namespace klbotlib.Extensions
{
    public static class ByteArrayExtension
    {
        private static readonly StringBuilder _sb = new StringBuilder();
        public static string ToHexString(this byte[] buffer)
        {
            return string.Join(string.Empty, buffer.Select(b => b.ToString("x2")));
        }
    }
}
