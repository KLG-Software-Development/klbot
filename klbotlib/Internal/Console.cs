#pragma warning disable CS1591 // 缺少对公共可见类型或成员的 XML 注释
using klbotlib.Extensions;
using System;
using System.Collections.Generic;

namespace Gleee.Consoleee;

/// <summary>
/// 增强控制台类
/// </summary>
public class Consoleee
{
    private int _bufferCursor = 0;
    private readonly List<char> _inputCharBuffer = new();
    public string InputBuffer { get => new(_inputCharBuffer.ToArray()); }
    private int _indentLevel = 0;
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
    public void Write(object obj) => Console.Write(" | ".Repeat(_indentLevel) + obj.ToString());
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
    public void WriteLn(object obj) => Console.WriteLine(" | ".Repeat(_indentLevel) + obj.ToString());
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
        Write($"[Task{_indentLevel}]", ConsoleColor.Magenta);
        WriteWithoutIndent(description + "...\n");
        _indentLevel++;
        operation();
        _indentLevel--;
        Write($"[Task{_indentLevel}]", ConsoleColor.Magenta);
        WriteWithoutIndent(TaskFinishMessage + "\n", ConsoleColor.Green);
    }
    [Obsolete("已过时")]
    public bool WriteOperationLn(string description, Func<bool> operation, ConsoleMessageType errorLevel = ConsoleMessageType.Default, string successString = "[Success]", string failedString = "[Failed]", ConsoleColor successColor = ConsoleColor.Green, ConsoleColor failedColor = ConsoleColor.Red)
    {
        WriteLn(description + "...", errorLevel);
        bool result = operation();
        Write(description);
        if (result)
            WriteLn(successString, successColor);
        else
            WriteLn(failedString, failedColor);
        return result;
    }
    public void ClearCurrentLine()
    {
        int tempTop = Console.CursorTop;
        Console.CursorVisible = false;
        Console.SetCursorPosition(0, Console.CursorTop);
        Console.Write(new string(' ', Console.WindowWidth - 1));
        Console.SetCursorPosition(0, tempTop);
        Console.CursorVisible = true;
    }
    public void OverwriteSecondLastLine(object obj, ConsoleMessageType level = ConsoleMessageType.Default, string prefix = "")
    {
        int tmpTop = Console.CursorTop, tmpLeft = Console.CursorLeft;
        Console.SetCursorPosition(0, Console.CursorTop - 1);
        Console.Write(new string(' ', Console.WindowWidth - 1));
        Console.SetCursorPosition(0, Console.WindowWidth - 1);
        Write(obj, level, prefix);
        Console.SetCursorPosition(tmpLeft, tmpTop);
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
                if (_bufferCursor != 0)
                {
                    Console.CursorVisible = false;
                    int c = Console.CursorLeft;
                    Console.Write(string.Concat("\b", InputBuffer.AsSpan(_bufferCursor), " "));
                    Console.SetCursorPosition(c - 1, Console.CursorTop);
                    Console.CursorVisible = true;
                    _bufferCursor--;
                    _inputCharBuffer.RemoveAt(_bufferCursor);
                }
            }
            else if (key.Key == ConsoleKey.LeftArrow)
            {
                if (_bufferCursor != 0)
                {
                    _bufferCursor--;
                    Console.SetCursorPosition(Console.CursorLeft - 1, Console.CursorTop);
                }
            }
            else if (key.Key == ConsoleKey.RightArrow)
            {
                if (_bufferCursor != InputBuffer.Length)
                {
                    _bufferCursor++;
                    Console.SetCursorPosition(Console.CursorLeft + 1, Console.CursorTop);
                }
            }
            else if (key.KeyChar is (>= ' ' and not ((char)127)) or '\t')
            {
                Console.CursorVisible = false;
                int tmp_left = Console.CursorLeft;
                Console.Write(key.KeyChar + InputBuffer[_bufferCursor..]);
                Console.SetCursorPosition(tmp_left + 1, Console.CursorTop);
                Console.CursorVisible = true;
                _inputCharBuffer.Insert(_bufferCursor, key.KeyChar);
                _bufferCursor++;
            }
        }
        string result = InputBuffer;
        ClearInputBuffer();
        return result;
    }
    public void ClearInputBuffer()
    {
        _inputCharBuffer.Clear();
        _bufferCursor = 0;
    }

    public Consoleee(string taskFinishMsg = "Operation done.")
    {
        TaskFinishMessage = taskFinishMsg;
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