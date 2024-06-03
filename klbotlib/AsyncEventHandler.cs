using System.Threading.Tasks;

namespace klbotlib;

/// <summary>
/// 异步事件处理
/// </summary>
public delegate Task AsyncEventHandler<TEventArgs>(object? sender, TEventArgs e);
