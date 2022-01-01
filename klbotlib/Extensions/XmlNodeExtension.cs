#pragma warning disable CS1591
using System.Xml;

namespace klbotlib.Extensions
{
    public static class XmlNodeExtension
    {
        public static bool TryGetFirstChildNodeByAttribute(this XmlNode node, string attribute_name, string attribute_value, out XmlNode output)
        {
            foreach (XmlNode child in node.ChildNodes)
            {
                if (child.Attributes != null && 
                    child.Attributes[attribute_name] != null && 
                    child.Attributes[attribute_name].Value == attribute_value)
                {
                    output = child;
                    return true;
                }
            }
            output = null;
            return false;
        }
    }
}
