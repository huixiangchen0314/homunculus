-- cljh_runtime.lua
local M = {}

-- 全局注册表：名称 -> 接口/类对象
M.interfaces = {}   -- 接口注册表，按全限定名索引
M.classes = {}      -- 类注册表，按全限定名索引

-- 定义接口，支持继承多个父接口
-- name    : 全限定名字符串，例如 "clojure.lang.IObj" 或 "my.module.Shape"
-- parents : 父接口对象或父接口对象数组（可选）
function M.interface(name, parents)
    if M.interfaces[name] then
        return M.interfaces[name]  -- 已存在，直接返回全局唯一实例
    end

    parents = parents or {}
    if type(parents) ~= "table" or getmetatable(parents) == nil then
        parents = { parents }
    end

    local proto = {
        _cljh_name = name,          -- 全限定名
        _cljh_methods = {},         -- 方法集合
        _cljh_parents = parents,    -- 父接口列表
    }

    M.interfaces[name] = proto     -- 注册到全局表
    return proto
end

-- 注册接口方法
function M.method(proto, method_name)
    proto._cljh_methods[method_name] = true
end

-- 定义类，使用全限定名
function M.class(name, base)
    if M.classes[name] then
        return M.classes[name]     -- 已存在，直接返回全局唯一实例
    end

    local cls = {
        _cljh_name = name,
        _cljh_base = base,
        _cljh_interfaces = {},
    }
    cls.__index = cls
    cls.new = function(...)
        local obj = setmetatable({}, cls)
        if cls._cljh_init then cls._cljh_init(obj, ...) end
        return obj
    end

    setmetatable(cls, {
        __call = function(self, ...)
            return self.new(...)
        end
    })

    M.classes[name] = cls          -- 注册到全局表
    return cls
end

-- 类实现接口
function M.implements(cls, proto)
    cls._cljh_interfaces[proto] = true
end

-- 内部函数：检查接口及其父接口是否在集合中
local function has_interface(interfaces, proto, visited)
    if interfaces[proto] then
        return true
    end
    visited = visited or {}
    if visited[proto] then return false end
    visited[proto] = true
    for _, parent in ipairs(proto._cljh_parents or {}) do
        if has_interface(interfaces, parent, visited) then
            return true
        end
    end
    return false
end

-- 检查对象是否满足接口（包含接口继承）
function M.satisfies(obj, proto)
    local cls = getmetatable(obj)
    while cls do
        if cls._cljh_interfaces and has_interface(cls._cljh_interfaces, proto) then
            return true
        end
        cls = cls._cljh_base
    end
    return false
end

-- 可选：按全限定名查找接口/类，支持反射
function M.get_interface(name)
    return M.interfaces[name]
end

function M.get_class(name)
    return M.classes[name]
end

return M