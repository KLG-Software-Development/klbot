using System;
using System.Collections.Generic;
using Newtonsoft.Json;

namespace klbotlib
{
    public class JMiraiResponse
    {
        public int code;
        public string msg;

        public void CheckStatusCode()
        {
            if (code != 0)
                throw new Exception($"mirai错误[{code}]：{msg}");
        }
    }

}
