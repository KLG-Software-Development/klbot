using Newtonsoft.Json;
using System;
using System.Linq;
using System.Net.Http;
using System.Net.Http.Headers;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;
using static System.Runtime.InteropServices.JavaScript.JSType;

namespace klbotlib.Modules;

///聊天bot模块
public class ChatXiaoIceModule : SingleTypeModule<MessagePlain>
{
    private const string _digits = "0123456789abcdef";
    [ModuleSetup]
    private int _bitLength = 256;
    [ModuleSetup]
    private string _conversationId = "ce1d6f9b-26c8-4ae2-9353-3f665c2ca411";
    [ModuleSetup]
    private string _traceId = "634382b22783467fa57f45e7539c8152";
    [ModuleSetup]
    private string _password = "3d9d5f16-5df0-43d7-902e-19274eecdc41";
    private static readonly byte[] _sBox = {
        0x63,0x7c,0x77,0x7b,0xf2,0x6b,0x6f,0xc5,0x30,0x01,0x67,0x2b,0xfe,0xd7,0xab,0x76,
        0xca,0x82,0xc9,0x7d,0xfa,0x59,0x47,0xf0,0xad,0xd4,0xa2,0xaf,0x9c,0xa4,0x72,0xc0,
        0xb7,0xfd,0x93,0x26,0x36,0x3f,0xf7,0xcc,0x34,0xa5,0xe5,0xf1,0x71,0xd8,0x31,0x15,
        0x04,0xc7,0x23,0xc3,0x18,0x96,0x05,0x9a,0x07,0x12,0x80,0xe2,0xeb,0x27,0xb2,0x75,
        0x09,0x83,0x2c,0x1a,0x1b,0x6e,0x5a,0xa0,0x52,0x3b,0xd6,0xb3,0x29,0xe3,0x2f,0x84,
        0x53,0xd1,0x00,0xed,0x20,0xfc,0xb1,0x5b,0x6a,0xcb,0xbe,0x39,0x4a,0x4c,0x58,0xcf,
        0xd0,0xef,0xaa,0xfb,0x43,0x4d,0x33,0x85,0x45,0xf9,0x02,0x7f,0x50,0x3c,0x9f,0xa8,
        0x51,0xa3,0x40,0x8f,0x92,0x9d,0x38,0xf5,0xbc,0xb6,0xda,0x21,0x10,0xff,0xf3,0xd2,
        0xcd,0x0c,0x13,0xec,0x5f,0x97,0x44,0x17,0xc4,0xa7,0x7e,0x3d,0x64,0x5d,0x19,0x73,
        0x60,0x81,0x4f,0xdc,0x22,0x2a,0x90,0x88,0x46,0xee,0xb8,0x14,0xde,0x5e,0x0b,0xdb,
        0xe0,0x32,0x3a,0x0a,0x49,0x06,0x24,0x5c,0xc2,0xd3,0xac,0x62,0x91,0x95,0xe4,0x79,
        0xe7,0xc8,0x37,0x6d,0x8d,0xd5,0x4e,0xa9,0x6c,0x56,0xf4,0xea,0x65,0x7a,0xae,0x08,
        0xba,0x78,0x25,0x2e,0x1c,0xa6,0xb4,0xc6,0xe8,0xdd,0x74,0x1f,0x4b,0xbd,0x8b,0x8a,
        0x70,0x3e,0xb5,0x66,0x48,0x03,0xf6,0x0e,0x61,0x35,0x57,0xb9,0x86,0xc1,0x1d,0x9e,
        0xe1,0xf8,0x98,0x11,0x69,0xd9,0x8e,0x94,0x9b,0x1e,0x87,0xe9,0xce,0x55,0x28,0xdf,
        0x8c,0xa1,0x89,0x0d,0xbf,0xe6,0x42,0x68,0x41,0x99,0x2d,0x0f,0xb0,0x54,0xbb,0x16
    };
    private static readonly byte[,] _rCon = 
    {
        { 0, 0, 0, 0},
        { 1, 0, 0, 0},
        { 2, 0, 0, 0},
        { 4, 0, 0, 0},
        { 8, 0, 0, 0},
        { 16, 0, 0, 0},
        { 32, 0, 0, 0},
        { 64, 0, 0, 0},
        { 128, 0, 0, 0},
        { 27, 0, 0, 0},
        { 54, 0, 0, 0}
    };

    private const string _url = "https://www.bing.com/english/zochatv2?cc=cn&ensearch=0";
    private static readonly HttpClient _client = new();
    private readonly StringBuilder _sb = new();
    private readonly Random _ro = new();

    /// <inheritdoc/>
    public sealed override bool IsTransparent => false;
    /// <inheritdoc/>
    public sealed override bool UseSignature => false;
    /// <inheritdoc/>
    public sealed override bool IsAsync => false;
    /// <inheritdoc/>
    public sealed override string FriendlyName => "微软小冰模块";
    /// <inheritdoc/>
    public sealed override string? Filter(MessagePlain msg) 
        => msg.TargetID.Count == 1 && msg.TargetID.Contains(HostBot.SelfID) 
        ? "ok" 
        : null;
    /// <inheritdoc/>
    public sealed override async Task<string> Processor(MessagePlain msg, string? _)
    {
        string normalizedQuery = AesEncrypt(msg.Text, _password, _bitLength);
        //JChatterBotRequest requestObj = new(_conversationId, new JQuery(normalizedQuery), _traceId);
        string s = "{\"conversationId\":\"" + _conversationId + "\",\"query\":{\"NormalizedQuery\":\"" + normalizedQuery + "\"},\"from\":\"chatbox\",\"traceId\":\"" + _traceId + "\"}";
        StringContent jsonAsPlainText = new(s);
        using (jsonAsPlainText)
        {
            HttpRequestMessage request = new();
            request.Method = HttpMethod.Post;
            request.RequestUri = new Uri(_url);
            request.Content = jsonAsPlainText;
            request.Headers.AcceptCharset.Add(new("Sec-CH-UA-Arch"));
            request.Headers.AcceptCharset.Add(new("Sec-CH-UA-Bitness"));
            request.Headers.AcceptCharset.Add(new("Sec-CH-UA-Full-Version"));
            request.Headers.AcceptCharset.Add(new("Sec-CH-UA-Mobile"));
            request.Headers.AcceptCharset.Add(new("Sec-CH-UA-Model"));
            request.Headers.AcceptCharset.Add(new("Sec-CH-UA-Platform"));
            request.Headers.AcceptCharset.Add(new("Sec-CH-UA-Platform-Version"));
            request.Headers.Accept.Add(new ("*/*"));
            request.Headers.AcceptLanguage.ParseAdd(new("zh-CN,zh;q=0.8,zh-TW;q=0.7,zh-HK;q=0.5,en-US;q=0.3,en;q=0.2"));
            request.Headers.AcceptEncoding.ParseAdd(new("br"));
            request.Headers.Referrer = new Uri("https://www.bing.com/search?q=c&qs=HS&sc=8-0&cvid=2B5E29D953D249AB9AADB000E841C382&FORM=QBLH&sp=1");
            ;
            HttpResponseMessage response = await _client.PostAsync(_url, jsonAsPlainText);
            response.EnsureSuccessStatusCode();
            string jreply = await response.Content.ReadAsStringAsync();
            return JsonConvert.DeserializeObject<JChatterBotReply>(jreply).content;
        }
    }
    //AES加密
    private string AesEncrypt(string plain, string password, int bitLength)
    {
        plain = StringPreprocess(plain);
        password = StringPreprocess(password);
        if (!(bitLength == 128 || bitLength == 192 || bitLength == 256))
            throw new ArgumentException("不支持的AES位宽");
        byte[] buffer = Encoding.ASCII.GetBytes(password)[..32];
        for (int i = 0; i < buffer.Length; i++)
        {
            buffer[i] = JavascriptIsNaN(password[i]) ? (byte)0 : (byte)password[i];
        }
        byte[] halfL = I(buffer, R_ExtendKey(buffer));
        byte[] l = new byte[halfL.Length * 2];
        Array.Copy(halfL, 0, l, 0, halfL.Length);
        Array.Copy(halfL, 0, l, 16, halfL.Length);
        byte[] h = new byte[16];
        long k = (long)DateTime.UtcNow.Subtract(DateTime.UnixEpoch).TotalMilliseconds;
        long tt = k % 1000;
        long it = k / 1000;
        int rt = (int)(_ro.NextDouble() * 65535);
        for (int f = 0; f < 2; f++) 
            h[f] = RshU(tt, f * 8 & 255);
        for (int f = 0; f < 2; f++) 
            h[f + 2] = RshU(rt, f * 8 & 255);
        for (int f = 0; f < 4; f++) 
            h[f + 4] = RshU(it, f * 8 & 255);
        _sb.Clear();
        for (int f = 0; f < 8; f++)
            _sb.Append((char)h[f]);
        string w = _sb.ToString();
        var ut = R_ExtendKey(l);
        int b = Convert.ToInt32(Math.Ceiling(plain.Length / 16.0));
        string[] d = new string[b];
        for (int i = 0; i < b; i++)
        {
            for (int e = 0; e < 4; e++)
                h[15 - e] = RshU(i, e * 8 & 255);
            for (int e = 0; e < 4; e++)
                h[11 - e] = RshU(i / 4294967296, e * 8);
            var ft = I(h, ut);
            int g = i < b - 1 
                ? 16 
                : (plain.Length - 1) % 16 + 1;
            char[] p = new char[g];
            for (int f = 0; f < g; f++)
            {
                p[f] = (char)(ft[f] ^ plain[i * 16 + f]);
                //p[f] = String.fromCharCode(p[f]);
            }
            d[i] = new string(p);
        }
        string nt = w + string.Join(string.Empty, d);
        return V_ToHexString(nt);
    }

    private string StringPreprocess(string s)
    {
        _sb.Clear();
        char[] cs = s.ToCharArray();
        for (int i = 0; i < s.Length; i++)
        {
            char c = cs[i];
            if (0x0080 <= c && c <= 0x07ff)
            {
                _sb.Append((char)(192 | c >> 6));
                _sb.Append((char)(128 | c & 63));
            }
            else if (0x0800 <= c && c <= 0xffff)
            {
                _sb.Append((char)(224 | c >> 12));
                _sb.Append((char)(128 | c >> 6 & 63));
                _sb.Append((char)(128 | c & 63));
            }
            else
                _sb.Append(c);
        }
        return _sb.ToString();
    }
    private byte[] GetKeyArray(string password)
    {
        password = StringPreprocess(password);
        byte[] buffer = Encoding.ASCII.GetBytes(password)[..32];
        for (int i = 0; i < buffer.Length; i++)
        {
            buffer[i] = JavascriptIsNaN(password[i]) ? (byte)0 : (byte)password[i];
        }
        return buffer;
    }
    //AES key schedule (Rijndael密钥生成)
    //详见https://en.wikipedia.org/wiki/AES_key_schedule
    private byte[,] R_ExtendKey(byte[] passwdArray)
    {
        int N = passwdArray.Length / 4;    // 将密码按4字节分组。一共32/4=8组
        int blockCountAdd6 = N + 6;        // 不知道为什么再加6组，现在一共8+6=14组
        byte[,] blocks = new byte[4 * (blockCountAdd6 + 1), 4];  // 不知道为什么又加了一组，最后总长为 4字节/组 * (14+1)组 = 60字节
        //对前8组赋值
        for (int b = 0; b < N; b++)
        {
            int offset = 4 * b;
            //前8组为直接按4字节顺序切片
            blocks[b, 0] = passwdArray[offset];
            blocks[b, 1] = passwdArray[offset + 1];
            blocks[b, 2] = passwdArray[offset + 2];
            blocks[b, 3] = passwdArray[offset + 3];
        }

        byte[] blockBuffer = new byte[4];   //u[]
        //对第8组以及之后的组赋值
        for (int b = N; b < 4 * (blockCountAdd6 + 1); b++)
        {
            //缓冲区先填入上一组的值
            for (int i = 0; i < 4; i++)
                blockBuffer[i] = blocks[b - 1, i];
            //对缓冲区的值进行预处理
            if (b % N == 0)    //若是每8组的开头第一组
            {
                blockBuffer = E_SBox(C_Rotate(blockBuffer));    //进行某种特定处理E(C(x))
                for (int i = 0; i < 4; i++)
                    blockBuffer[i] ^= _rCon[b / N, i]; //和某个数组L进行异或
            }
            else    //否则
            {
                if (N > 6 && b % N == 4)  //不知道为什么判断组数大于6 且 是每8组的开头第五组
                    blockBuffer = E_SBox(blockBuffer);   //进行某种特定处理E(x)
            }
            //组值等于编号偏移-8的组的值 与 缓冲区中的值异或
            for (int i = 0; i < 4; i++)
                blocks[b, i] = (byte)(blocks[b - N, i] ^ blockBuffer[i]);
        }
        return blocks;
    }
    private byte[,] T_Xor(byte[,] keyArraySlice4, byte[,] exKeyArray, int i, int r)
    {
        for (int u = 0; u < 4; u++)
        {
            for (int f = 0; f < r; f++)
                keyArraySlice4[u, f] ^= exKeyArray[i * 4 + f, u];
        }
        return keyArraySlice4;
    }
    private byte[] C_Rotate(byte[] n)
    {
        var first = n[0];
        for (int i = 0; i < 3; i++)
            n[i] = n[i + 1];
        n[3] = first;
        return n;
    }
    private byte[] E_SBox(byte[] n)
    {
        for (var t = 0; t < 4; t++)
            n[t] = _sBox[n[t]];
        return n;
    }
    private byte[,] U_SBox(byte[,] n, int t)
    {
        for (int i = 0; i < 4; i++)
            for (int r = 0; r < t; r++)
                n[i, r] = _sBox[n[i, r]];
        return n;
    }
    private string V_ToHexString(string n)
    {
        _sb.Clear();
        _sb.Append("0x");
        for (int t = 0; t < n.Length; t++)
        {
            _sb.Append(_digits[n[t] >> 4]);
            _sb.Append(_digits[n[t] & 15]);
        }
        return _sb.ToString();
    }
    private bool JavascriptIsNaN(char c)
        => double.IsNaN(c);
    //不知道什么意思的函数
    private byte[] I(byte[] keyArray, byte[,] exKeyArray)
    {
        int l = exKeyArray.GetLength(0) / 4 - 1;
        byte[,] keyArraySlice4 = new byte[4, 4];  //r
        //keyArray切成4片
        for (var i = 0; i < 4 * 4; i++)
            keyArraySlice4[i % 4, i / 4] = keyArray[i];
        //对r进行t处理
        keyArraySlice4 = T_Xor(keyArraySlice4, exKeyArray, 0, 4);
        for (int s = 1; s < l; s++)
        {
            keyArraySlice4 = U_SBox(keyArraySlice4, 4);
            keyArraySlice4 = F(keyArraySlice4, 4);
            keyArraySlice4 = H(keyArraySlice4, 4);
            keyArraySlice4 = T_Xor(keyArraySlice4, exKeyArray, s, 4);
        }
        keyArraySlice4 = U_SBox(keyArraySlice4, 4);
        keyArraySlice4 = F(keyArraySlice4, 4);
        keyArraySlice4 = T_Xor(keyArraySlice4, exKeyArray, l, 4);
        //降维
        byte[] c = new byte[4 * 4];
        for (int i = 0; i < 4 * 4; i++)
            c[i] = keyArraySlice4[i % 4, i / 4];
        return c;
    }
    private byte[,] F(byte[,] n, int t)
    {
        byte[] u = new byte[4];
        for (int r = 1; r < 4; r++)
        {
            for (int i = 0; i < 4; i++)
                u[i] = n[r, (i + r) % t];
            for (int i = 0; i < 4; i++)
                n[r, i] = u[i];
        }
        return n;
    }
    private byte[,] H(byte[,] n, int _)
    {
        for (int i = 0; i < 4; i++)
        {
            byte[] t = new byte[4];
            byte[] r = new byte[4];
            for (int u = 0; u < 4; u++)
            {
                t[u] = n[u, i];
                r[u] = (n[u, i] & 128) != 0 
                    ? (byte)(n[u, i] << 1 ^ 283) 
                    : (byte)(n[u, i] << 1);
            }
            n[0, i] = (byte)(r[0] ^ t[1] ^ r[1] ^ t[2] ^ t[3]);
            n[1, i] = (byte)(t[0] ^ r[1] ^ t[2] ^ r[2] ^ t[3]);
            n[2, i] = (byte)(t[0] ^ t[1] ^ r[2] ^ t[3] ^ r[3]);
            n[3, i] = (byte)(t[0] ^ r[0] ^ t[1] ^ t[2] ^ r[3]);
        }
        return n;
    }
    private byte RshU(dynamic b, int n)
        => (byte)((uint)b >> n);

    private class JChatterBotRequest
    {
        string conversationId;
        JQuery query;
        string traceId;
        string from = "chatbox";
        public JChatterBotRequest(string conversationId, JQuery query, string traceId)
        {
            this.conversationId = conversationId;
            this.query = query;
            this.traceId = traceId;
        }
    }
    private class JQuery
    {
        string NormalizedQuery;
        public JQuery(string normalizedQuery)
        {
            NormalizedQuery = normalizedQuery;
        }
    }
    private class JChatterBotReply
    {
        public string content = string.Empty;
    }
}