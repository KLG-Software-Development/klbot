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
    private object _lockObj = new();
    private int _indentLevel = 0;
    public string TaskFinishMessage { get; set; }
    public void Clear()
    {
        lock (_lockObj)
            Console.Clear();
    }
    public ConsoleKeyInfo ReadKey() => Console.ReadKey();
    public ConsoleKeyInfo ReadKey(object obj)
    {
        lock (_lockObj)
            WriteLn(obj);
        return Console.ReadKey();   //ReadKey()会阻塞线程，所以不能lock。否则会一直占用控制台
    }
    public void WriteWithoutIndent(object obj)
    {
        lock (_lockObj)
            Console.Write(obj.ToString());
    }
    public void WriteWithoutIndent(object obj, ConsoleColor c)
    {
        lock (_lockObj)
        {
            var tmp = Console.ForegroundColor;
            Console.ForegroundColor = c;
            Console.Write(obj.ToString());
            Console.ForegroundColor = tmp;
        }
    }
    public void Write(object obj)
    {
        lock (_lockObj)
            Console.Write(" | ".Repeat(_indentLevel) + obj.ToString());
    }
    public void Write(object obj, ConsoleColor c)
    {
        lock (_lockObj)
        {
            var tmp = Console.ForegroundColor;
            Console.ForegroundColor = c;
            Write(obj.ToNotNullString());
            Console.ForegroundColor = tmp;
        }
    }
    public void Write(object obj, ConsoleMessageType level = ConsoleMessageType.Default, string prefix = "")
    {
        lock (_lockObj)
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
                default:
                    throw new Exception($"Unknown errorlevel '{level}'");
            }
        }
    }
    public void WriteLn(object obj)
    {
        lock (_lockObj)
            Console.WriteLine(" | ".Repeat(_indentLevel) + obj.ToString());
    }
    public void WriteLn(object obj, ConsoleColor c)
    {
        lock (_lockObj)
        {
            var tmp = Console.ForegroundColor;
            Console.ForegroundColor = c;
            WriteLn(obj.ToNotNullString());
            Console.ForegroundColor = tmp;
        }
    }
    public void WriteLn(object obj, ConsoleMessageType level, string prefix = "")
    {
        lock (_lockObj)
            Write($"{obj}\n", level, prefix);
    }
    public void WriteNewLn(object obj) => WriteLn($"\n{obj}");
    public void WriteNewLn(object obj, ConsoleColor c) => WriteLn($"\n{obj}", c);
    public void WriteNewLn(object obj, ConsoleMessageType level) => WriteLn($"\n{obj}", level);
    public void WriteOperationLn(string description, Action operation)
    {
        lock (_lockObj)
        {
            Write($"[Task{_indentLevel}]", ConsoleColor.Magenta);
            WriteWithoutIndent($"{description}...\n");
            _indentLevel++;
            operation();
            _indentLevel--;
            Write($"[Task{_indentLevel}]", ConsoleColor.Magenta);
            WriteWithoutIndent($"{TaskFinishMessage}\n", ConsoleColor.Green);
        }
    }
    [Obsolete("已过时")]
    public bool WriteOperationLn(string description, Func<bool> operation, ConsoleMessageType errorLevel = ConsoleMessageType.Default, string successString = "[Success]", string failedString = "[Failed]", ConsoleColor successColor = ConsoleColor.Green, ConsoleColor failedColor = ConsoleColor.Red)
    {
        lock (_lockObj)
        {
            WriteLn($"{description}...", errorLevel);
            bool result = operation();
            Write(description);
            if (result)
                WriteLn(successString, successColor);
            else
                WriteLn(failedString, failedColor);
            return result;
        }
    }
    public void ClearCurrentLine()
    {
        lock (_lockObj)
        {
            int tempTop = Console.CursorTop;
            Console.CursorVisible = false;
            Console.SetCursorPosition(0, Console.CursorTop);
            Console.Write(new string(' ', Console.WindowWidth - 1));
            Console.SetCursorPosition(0, tempTop);
            Console.CursorVisible = true;
        }
    }
    public void OverwriteSecondLastLine(object obj, ConsoleMessageType level = ConsoleMessageType.Default, string prefix = "")
    {
        lock (_lockObj)
        {
            int tmpTop = Console.CursorTop, tmpLeft = Console.CursorLeft;
            Console.SetCursorPosition(0, Console.CursorTop - 1);
            Console.Write(new string(' ', Console.WindowWidth - 1));
            Console.SetCursorPosition(0, Console.WindowWidth - 1);
            Write(obj, level, prefix);
            Console.SetCursorPosition(tmpLeft, tmpTop);
        }
    }
    public void SetCursorPos(int left, int top)
    {
        lock (_lockObj)
            Console.SetCursorPosition(left, top);
    }
    public string ReadLn()
    {
        string? s = Console.ReadLine();
        if (s == null)
            return string.Empty;
        else
            return s;
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