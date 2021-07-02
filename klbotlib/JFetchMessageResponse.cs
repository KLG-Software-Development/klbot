using System;
using System.Collections.Generic;
using Newtonsoft.Json;

namespace klbotlib
{
    public class JFetchMessageResponse : JMiraiResponse
    {
        public List<JMessagePackage> data;
    }

}
