#pragma warning disable CS1591
using System;
using System.Collections.Generic;
using System.Text;

namespace klbotlib.Extensions
{
    public static class StringExtension
    {
        private readonly static StringBuilder _sb = new();

        /// <summary>
        /// 将超过指定长度限制的字符串的中间部分省略
        /// </summary>
        /// /// <param name="s">原始字符串</param>
        /// <param name="maxLength">长度上限</param>
        public static string Shorten(this string s, int maxLength)
        {
            if (s.Length > maxLength)
                return s.Substring(0, maxLength) + "...";
            else
                return s;
        }
        public static string Repeat(this string value, int count)
        {
            lock (_sb)
            {
                _sb.Clear();
                _sb.Insert(0, value, count);
                return _sb.ToString();
            }
        }
        private static readonly Dictionary<ConsoleColor, int> _ansiColorCode = new()
        {
            { ConsoleColor.Red, 31 },
            { ConsoleColor.Green, 32 },
            { ConsoleColor.Yellow, 33 },
            { ConsoleColor.Magenta, 35 },
            { ConsoleColor.Cyan, 36 },
        };
        public static string ToAnsiColor(this string value, ConsoleColor color)
        {
            return $"\x1b[{_ansiColorCode[color]}m{value}\x1b[0m";
        }
    }
}
