namespace klbotlib.Extensions;
/// <summary>
/// Object类的扩展方法
/// </summary>
public static class ObjectExtension
{
    /// <summary>
    /// 返回ToString()的非null版本
    /// </summary>
    public static string ToNotNullString(this object obj)
    {
        if (obj == null)
            return "\\NULL OBJECT\\";
        string? s = obj.ToString();
        if (s == null)
            return "\\NULL STRING\\";
        else
            return s;
    }
}
