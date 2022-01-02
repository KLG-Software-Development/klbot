using System;
using System.Collections.Generic;
using System.Drawing;
using System.Runtime.CompilerServices;
using System.Text;

namespace Gleee.Consoleee
{
    /// <summary>
    /// 增强控制台类
    /// </summary>
    public class Consoleee
    {
        private int buffer_cursor = 0;
        private readonly List<char> input_char_buffer = new List<char>();
        public string InputBuffer { get => new string(input_char_buffer.ToArray()); }
        private int indent_level = 0;
        public string TaskFinishMessage { get; set; }
        public void Clear() => Console.Clear();
        public ConsoleKeyInfo ReadKey() => Console.ReadKey();
        public ConsoleKeyInfo ReadKey(object obj)
        {
            WriteLn(obj);
            return Console.ReadKey();
        }
        public void WriteWithoutIndent(object obj)
        {
            Console.Write(obj.ToString());
        }
        public void WriteWithoutIndent(object obj, ConsoleColor c)
        {
            var tmp = Console.ForegroundColor;
            Console.ForegroundColor = c;
            Console.Write(obj.ToString());
            Console.ForegroundColor = tmp;
        }
        public void Write(object obj) => Console.Write(" | ".Repeat(indent_level) + obj.ToString());
        public void Write(object obj, ConsoleColor c)
        {
            var tmp = Console.ForegroundColor;
            Console.ForegroundColor = c;
            Write(obj.ToString());
            Console.ForegroundColor = tmp;
        }
        public void Write(object obj, ConsoleMessageType level = ConsoleMessageType.Default, string prefix = "")
        {
            Write(prefix);
            switch (level)
            {
                case ConsoleMessageType.Default:
                    Write(obj);
                    return;
                case ConsoleMessageType.Info:
                    Write($"[Info]", ConsoleColor.Cyan);
                    WriteWithoutIndent(obj);
                    return;
                case ConsoleMessageType.Warning:
                    Write($"[Warning]", ConsoleColor.Yellow);
                    WriteWithoutIndent(obj);
                    return;
                case ConsoleMessageType.Error:
                    Write($"[Error]", ConsoleColor.Red);
                    WriteWithoutIndent(obj);
                    return;
                case ConsoleMessageType.Task:
                    Write($"[Task]", ConsoleColor.Magenta);
                    WriteWithoutIndent(obj);
                    return;
            }
            throw new Exception($"Unknown errorlevel '{level}'");
        }
        public void WriteLn(object obj) => Console.WriteLine(" | ".Repeat(indent_level) + obj.ToString());
        public void WriteLn(object obj, ConsoleColor c)
        {
            var tmp = Console.ForegroundColor;
            Console.ForegroundColor = c;
            WriteLn(obj.ToString());
            Console.ForegroundColor = tmp;
        }
        public void WriteLn(object obj, ConsoleMessageType level, string prefix = "") => Write(obj.ToString() + "\n", level, prefix);
        public void WriteLnWithLock(object obj, ConsoleColor c)
        {
            lock (this)
            {
                WriteLn(obj, c);
            }
        }
        public void WriteLnWithLock(object obj, ConsoleMessageType level, string prefix = "")
        {
            lock (this)
            {
                Write(obj.ToString() + "\n", level, prefix);
            }
        } 
        public void WriteNewLn(object obj) => WriteLn("\n" + obj.ToString());
        public void WriteNewLn(object obj, ConsoleColor c) => WriteLn("\n" + obj.ToString(), c);
        public void WriteNewLn(object obj, ConsoleMessageType level) => WriteLn("\n" + obj.ToString(), level);
        public void WriteOperationLn(string description, Action operation)
        {
            Write($"[Task{indent_level}]", ConsoleColor.Magenta);
            WriteWithoutIndent(description + "...\n");
            indent_level++;
            operation();
            indent_level--;
            Write($"[Task{indent_level}]", ConsoleColor.Magenta);
            WriteWithoutIndent(TaskFinishMessage + "\n", ConsoleColor.Green);
        }
        [Obsolete]
        public bool WriteOperationLn(string description, Func<bool> operation, ConsoleMessageType error_level = ConsoleMessageType.Default, string success_string = "[Success]", string failed_string = "[Failed]", ConsoleColor success_color = ConsoleColor.Green, ConsoleColor failed_color = ConsoleColor.Red)
        {
            WriteLn(description + "...", error_level);
            bool result = operation();
            Write(description);
            if (result)
                WriteLn(success_string, success_color);
            else
                WriteLn(failed_string, failed_color);
            return result;
        }
        public void ClearCurrentLine()
        {
            int temp_top = Console.CursorTop;
            Console.CursorVisible = false;
            Console.SetCursorPosition(0, Console.CursorTop);
            Console.Write(new string(' ', Console.WindowWidth - 1));
            Console.SetCursorPosition(0, temp_top);
            Console.CursorVisible = true;
        }
        public void OverwriteSecondLastLine(object obj, ConsoleMessageType level = ConsoleMessageType.Default, string prefix = "")
        {
            int tmp_top = Console.CursorTop, tmp_left = Console.CursorLeft;
            Console.SetCursorPosition(0, Console.CursorTop - 1);
            Console.Write(new string(' ', Console.WindowWidth - 1));
            Console.SetCursorPosition(0, Console.WindowWidth - 1);
            Write(obj, level, prefix);
            Console.SetCursorPosition(tmp_left, tmp_top);
        }
        public void SetCursorPos(int left, int top) => Console.SetCursorPosition(left, top);
        public string ReadLn() => Console.ReadLine();
        public string BufferedReadLn()
        {
            while (true)
            {
                var key = Console.ReadKey(true);
                if (key.Key == ConsoleKey.Enter)
                {
                    Console.Write("\n");
                    break;
                }
                else if (key.Key == ConsoleKey.Backspace)
                {
                    if (buffer_cursor != 0)
                    {
                        Console.CursorVisible = false;
                        int c = Console.CursorLeft;
                        Console.Write("\b" + InputBuffer.Substring(buffer_cursor) + " ");
                        Console.SetCursorPosition(c - 1, Console.CursorTop);
                        Console.CursorVisible = true;
                        buffer_cursor--;
                        input_char_buffer.RemoveAt(buffer_cursor);
                    }
                }
                else if (key.Key == ConsoleKey.LeftArrow)
                {
                    if (buffer_cursor != 0)
                    {
                        buffer_cursor--;
                        Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop);
                    }
                }
                else if (key.Key == ConsoleKey.RightArrow)
                {
                    if (buffer_cursor != InputBuffer.Length)
                    {
                        buffer_cursor++;
                        Console.SetCursorPosition(Console.CursorLeft + 1, Console.CursorTop);
                    }
                }
                else if ((key.KeyChar >= ' ' && key.KeyChar != (char)127) || key.KeyChar == '\t')
                {
                    Console.CursorVisible = false;
                    int tmp_left = Console.CursorLeft;
                    Console.Write(key.KeyChar + InputBuffer.Substring(buffer_cursor));
                    Console.SetCursorPosition(tmp_left + 1, Console.CursorTop);
                    Console.CursorVisible = true;
                    input_char_buffer.Insert(buffer_cursor, key.KeyChar);
                    buffer_cursor++;
                }
            }
            string result = InputBuffer;
            ClearInputBuffer();
            return result;
        }
        public void ClearInputBuffer() 
        {
            input_char_buffer.Clear();
            buffer_cursor = 0;
        } 

        public Consoleee(string task_finish_msg = "Operation done.")
        {
            TaskFinishMessage = task_finish_msg;
        }
    }

    public enum ConsoleMessageType
    { Default, Info, Warning, Error, Task }

    public interface IConsole
    {
        int CursorTop { get; set; }
        void Write(object obj);
        char Read();
        string ReadLn();
        void ReadKey();
        ConsoleColor ForeColor { get; set; }
        ConsoleColor BackColor { get; set; }
        void SetCursorPos(int left, int top);
    }

    static class StringExtension
    {
        public static string Repeat(this string value, int count)
        {
            return new StringBuilder(value.Length * count).Insert(0, value, count).ToString();
        }
    }
}