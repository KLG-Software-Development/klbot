using klbotlib.Exceptions;
using klbotlib.Modules;
using System;
using System.Collections;
using System.Collections.Generic;

namespace klbotlib
{
    /// <summary>
    /// 模块链条类。实现了IEnumerable接口
    /// </summary>
    public class ModuleChain : IEnumerable<Module>
    {
        private readonly Dictionary<string, int> _indexById = new Dictionary<string, int>();
        private readonly Dictionary<string, int> _moduleCountByName = new Dictionary<string, int>();
        private readonly List<Module> _modules = new List<Module>();

        IEnumerator<Module> IEnumerable<Module>.GetEnumerator() => _modules.GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => _modules.GetEnumerator();
        //计算模块ID
        internal string CalcModuleID(string module_name, int moduleIndex)
        {
            if (moduleIndex == 0)
                return module_name;
            else
                return $"{module_name}#{moduleIndex}";
        }
        //计算模块ID
        internal string CalcModuleID(Module module)
        {
            string name = module.ModuleName;
            if (!_moduleCountByName.TryGetValue(name, out int count))
                count = 0;
            return CalcModuleID(name, count);
        }
        //下面两个方法为internal，阻止任何绕过KLBot直接修改ModuleChain的行为。这是因为需要KLBot帮助Module和ModuleChain沟通传递ModuleID信息。
        /// <summary>
        /// 向模块链条中添加模块
        /// </summary>
        /// <param name="modules"></param>
        internal void AddModule(params Module[] modules)
        {
            foreach (var m in modules)
            {
                _indexById.Add(m.ModuleID, this._modules.Count);      //添加ID到ID-索引字典
                if (!_moduleCountByName.ContainsKey(m.ModuleName))    //递增模块类型-数量字典
                    _moduleCountByName.Add(m.ModuleName, 1);
                else
                    _moduleCountByName[m.ModuleName]++;
                this._modules.Add(m);
            }
        }

        /// <summary>
        /// 返回模块链条中是否含有某个ID的模块
        /// </summary>
        /// <param name="moduleId">模块ID</param>
        public bool ContainsModule(string moduleId) => _moduleCountByName.ContainsKey(moduleId);
        /// <summary>
        /// 模块链条中模块的数量
        /// </summary>
        public int Count { get => _modules.Count; }
        /// <summary>
        /// 获取模块实例
        /// </summary>
        /// <param name="moduleId">模块ID</param>
        public Module this[string moduleId]
        {
            get
            {
                if (!_indexById.ContainsKey(moduleId))
                    throw new ModuleMissingException($"在模块链条中不存在模块\"{moduleId}\"");
                return _modules[_indexById[moduleId]];
            }
            set
            {
                if (!_indexById.ContainsKey(moduleId))
                    throw new ModuleMissingException($"在模块链条中不存在模块\"{moduleId}\"");
                _modules[_moduleCountByName[moduleId]] = value;
            }
        }
        /// <summary>
        /// 获取模块实例。当你不关心目标模块的类型时，使用此方法。等价于直接使用ModuleChain[module_id]。
        /// 成功时会返回true，失败时会返回false。
        /// </summary>
        /// <param name="moduleId">模块ID</param>
        /// <param name="module">获取到的模块对象。失败时为null</param>
        public bool TryGetModule(string moduleId, out Module module)
        {
            if (!_indexById.ContainsKey(moduleId))
            {
                module = null;
                return false;
            }
            else
            {
                module = _modules[_indexById[moduleId]];
                return true;
            }
        }
        /// <summary>
        /// 获取模块实例。当你需要获取某个类型的特定模块时，使用此方法。
        /// 成功时会返回true，失败时会返回false。
        /// </summary>
        /// <typeparam name="T">目标模块的类型</typeparam>
        /// <param name="index">目标模块的索引。默认为0</param>
        /// <param name="module">目标模块对象。失败时为null</param>
        public bool TryGetModule<T>(int index, out T module) where T : Module
        {
            string moduleId = CalcModuleID(typeof(T).Name, index);
            if (!_indexById.ContainsKey(moduleId))
            {
                module = null;
                return false;
            }
            else if (_modules[_indexById[moduleId]] is T tmodule)
            {
                module = tmodule;
                return true;
            }
            else
            {
                module = null;
                return true;
            }
        }
        /// <summary>
        /// 获取模块实例。当你不关心目标模块的类型时，使用此方法。等价于直接使用ModuleChain[module_id]。
        /// </summary>
        /// <param name="moduleId">模块ID</param>
        public Module GetModule(string moduleId) => this[moduleId];
        /// <summary>
        /// 获取模块实例。当你需要获取某个类型的特定模块时，使用此方法。
        /// </summary>
        /// <typeparam name="T">目标模块的类型</typeparam>
        /// <param name="moduleIndex">目标模块的索引。默认为0</param>
        public T GetModule<T>(int moduleIndex = 0) where T : Module
        {
            return (T)this[CalcModuleID(typeof(T).Name, moduleIndex)];
        }
        ///<inheritdoc/>
        public void ForEach(Action<Module> action) => _modules.ForEach(action);
    }
}
