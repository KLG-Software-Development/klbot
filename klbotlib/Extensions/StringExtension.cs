#pragma warning disable CS1591
namespace klbotlib.Extensions
{
    public static class StringExtension
    {
        /// <summary>
        /// 将超过指定长度限制的字符串的中间部分省略
        /// </summary>
        /// /// <param name="s">原始字符串</param>
        /// <param name="max_length">长度上限</param>
        public static string Shorten(this string s, int max_length)
        {
            if (s.Length > max_length)
                return s.Substring(0, max_length) + "...";
            else
                return s;
        }
    }
#pragma warning restore CS1591 
}
