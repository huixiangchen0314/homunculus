-- cljh_PersistArrayMap.lua
-- 不可变数组映射（小映射优化），模拟 Clojure 的 PersistentArrayMap
-- 键值对以扁平数组存储，线性查找。元素数达到阈值后由外部转换为哈希映射。

local PersistArrayMap = {}
PersistArrayMap.__index = PersistArrayMap

-- 常量：当数组长度 >= 16（8 个键值对）时建议升级为哈希映射
PersistArrayMap.HASHTABLE_THRESHOLD = 16
-- 空实例
PersistArrayMap.EMPTY = nil  -- 先声明，稍后赋值

--------------------
-- 构造函数
--------------------
-- 创建一个新的 PersistentArrayMap
-- @param array 扁平数组 {key1, val1, key2, val2, ...}
-- @param meta  元数据映射（可选）
function PersistArrayMap:new(array, meta)
    local obj = {
        _array = array or {},
        _meta = meta or nil
    }
    setmetatable(obj, self)
    return obj
end

-- 空映射
PersistArrayMap.EMPTY = PersistArrayMap:new({})

--------------------
-- 静态工厂方法
--------------------
-- 从 Lua 表创建（{k1, v1, k2, v2, ...} 或 {{k,v}, ...}）
function PersistArrayMap.from_table(tbl)
    local arr = {}
    if tbl[1] and type(tbl[1]) == "table" then
        for _, pair in ipairs(tbl) do
            arr[#arr+1] = pair[1]
            arr[#arr+1] = pair[2]
        end
    else
        for i = 1, #tbl, 2 do
            arr[#arr+1] = tbl[i]
            arr[#arr+1] = tbl[i+1]
        end
    end
    return PersistArrayMap:new(arr)
end

-- 带重复检查的创建（如 Clojure 的 createWithCheck）
function PersistArrayMap.create_with_check(init)
    for i = 1, #init, 2 do
        for j = i + 2, #init, 2 do
            if PersistArrayMap.equal_key(init[i], init[j]) then
                error("Duplicate key: " .. tostring(init[i]))
            end
        end
    end
    return PersistArrayMap:new(init)
end

-- 模拟 createAsIfByAssoc（处理尾部额外值和重复键）
function PersistArrayMap.create_as_if_by_assoc(init)
    local has_trailing = (#init % 2 == 1)
    local complex_path = has_trailing

    for i = 1, #init, 2 do
        for j = 1, i-1, 2 do
            if PersistArrayMap.equal_key(init[i], init[j]) then
                complex_path = true
                break
            end
        end
        if complex_path then break end
    end

    if complex_path then
        return PersistArrayMap._create_as_if_by_assoc_complex(init, has_trailing)
    else
        return PersistArrayMap:new(init)
    end
end

function PersistArrayMap._create_as_if_by_assoc_complex(init, has_trailing)
    if has_trailing then
        -- 尾部值作为列表，这里简化为追加到数组
        local trailing = init[#init]
        local new_init = {}
        for i = 1, #init - 1 do
            new_init[i] = init[i]
        end
        -- 假设 trailing 是一个可序列，我们这里简单处理：若为表且包含键值对
        if type(trailing) == "table" and trailing[1] and type(trailing[1]) == "table" then
            for _, pair in ipairs(trailing) do
                new_init[#new_init+1] = pair[1]
                new_init[#new_init+1] = pair[2]
            end
        end
        init = new_init
    end

    -- 去重并保留最后一个值
    local nodups = {}
    local seen = {}
    for i = 1, #init, 2 do
        seen[init[i]] = i
    end
    local m = 0
    for i = 1, #init, 2 do
        if seen[init[i]] == i then  -- 最后一个出现的索引
            nodups[m+1] = init[i]
            nodups[m+2] = init[i+1]
            m = m + 2
        end
    end
    return PersistArrayMap:new(nodups)
end

--------------------
-- 内部工具
--------------------
-- 线性查找键的索引（从 1 开始），未找到返回 0（与 Java 负数不同，但便于判断）
function PersistArrayMap.indexOf(self, key)
    local array = self._array
    -- 关键字可直接用 == 比较（Lua 中引用相等）
    for i = 1, #array, 2 do
        if key == array[i] then
            return i
        end
    end
    return 0
end

-- 键相等判断（模拟 Java 的 equalKey）
function PersistArrayMap.equal_key(k1, k2)
    return k1 == k2
end

-- 是否建议升级（达到阈值）
function PersistArrayMap:needs_upgrade()
    return #self._array >= PersistArrayMap.HASHTABLE_THRESHOLD
end

-- 升级用（默认不实现，由外部覆盖）
function PersistArrayMap:createHT(array)
    error("Upgrade to hash map not implemented. Set PersistArrayMap.createHT_factory to enable.")
end

-- 可设置的升级工厂
PersistArrayMap.createHT_factory = nil

-- 重新定义升级方法，让外部可通过设置工厂实现无缝转换
function PersistArrayMap:createHT(array)
    if PersistArrayMap.createHT_factory then
        return PersistArrayMap.createHT_factory(self._meta, array)
    else
        -- 默认行为：返回一个新的 PersistentArrayMap 但标记需要升级
        -- 实际中外部应检测并调用工厂
        return PersistArrayMap:new(array, self._meta)
    end
end

--------------------
-- 核心接口
--------------------
function PersistArrayMap:count()
    return math.floor(#self._array / 2)
end

function PersistArrayMap:capacity()
    return self:count()
end

function PersistArrayMap:contains_key(key)
    return self:indexOf(key) > 0
end

function PersistArrayMap:entry_at(key)
    local i = self:indexOf(key)
    if i > 0 then
        return { key = self._array[i], val = self._array[i+1] }
    end
    return nil
end

function PersistArrayMap:val_at(key, not_found)
    local i = self:indexOf(key)
    if i > 0 then
        return self._array[i+1]
    else
        return not_found
    end
end

-- assoc 添加/更新键
function PersistArrayMap:assoc(key, val)
    local i = self:indexOf(key)
    local array = self._array
    if i > 0 then
        -- 已存在，替换值
        if array[i+1] == val then
            return self  -- 值未变，返回自身
        end
        local new_array = {}
        for j = 1, #array do
            if j == i+1 then
                new_array[j] = val
            else
                new_array[j] = array[j]
            end
        end
        return PersistArrayMap:new(new_array, self._meta)
    else
        -- 新增键
        if #array >= PersistArrayMap.HASHTABLE_THRESHOLD then
            -- 达到阈值，应升级为哈希映射，这里调用 createHT 并委托
            local ht = self:createHT(array)
            return ht:assoc(key, val)
        end
        local new_array = {}
        for j = 1, #array do
            new_array[j] = array[j]
        end
        new_array[#new_array+1] = key
        new_array[#new_array+1] = val
        return PersistArrayMap:new(new_array, self._meta)
    end
end

-- assocEx 严格添加，键已存在则报错
function PersistArrayMap:assoc_ex(key, val)
    local i = self:indexOf(key)
    if i > 0 then
        error("Key already present")
    end
    if #self._array >= PersistArrayMap.HASHTABLE_THRESHOLD then
        local ht = self:createHT(self._array)
        return ht:assoc_ex(key, val)
    end
    local new_array = {}
    -- 新键添加到开头（Java 的 assocEx 放在前面）
    new_array[1] = key
    new_array[2] = val
    for j = 1, #self._array do
        new_array[j+2] = self._array[j]
    end
    return PersistArrayMap:new(new_array, self._meta)
end

-- without 移除键
function PersistArrayMap:without(key)
    local i = self:indexOf(key)
    if i > 0 then
        local new_len = #self._array - 2
        if new_len == 0 then
            return self:empty()
        end
        local new_array = {}
        -- 复制前半部分
        for j = 1, i-1 do
            new_array[j] = self._array[j]
        end
        -- 复制后半部分
        for j = i+2, #self._array do
            new_array[j-2] = self._array[j]
        end
        return PersistArrayMap:new(new_array, self._meta)
    else
        return self  -- 键不存在，返回自身
    end
end

-- empty 返回空映射（保留元数据）
function PersistArrayMap:empty()
    if self._meta then
        return PersistArrayMap.EMPTY:with_meta(self._meta)
    else
        return PersistArrayMap.EMPTY
    end
end

-- 元数据
function PersistArrayMap:meta()
    return self._meta
end

function PersistArrayMap:with_meta(meta)
    if self._meta == meta then
        return self
    end
    return PersistArrayMap:new(self._array, meta)
end

--------------------
-- 序列与迭代
--------------------
-- 返回一个 Seq 对象（模拟 ISeq）
function PersistArrayMap:seq()
    if #self._array == 0 then
        return nil
    end
    return PersistArrayMap.Seq:new(self._array, 1)
end

-- MapEntry 序列
PersistArrayMap.Seq = {}
PersistArrayMap.Seq.__index = PersistArrayMap.Seq

function PersistArrayMap.Seq:new(array, index, meta)
    local obj = {
        array = array,
        index = index,
        meta = meta
    }
    setmetatable(obj, self)
    return obj
end

function PersistArrayMap.Seq:first()
    return { key = self.array[self.index], val = self.array[self.index+1] }
end

function PersistArrayMap.Seq:next()
    if self.index + 2 <= #self.array then
        return PersistArrayMap.Seq:new(self.array, self.index + 2)
    else
        return nil
    end
end

function PersistArrayMap.Seq:count()
    return math.floor((#self.array - self.index + 1) / 2)
end

-- 迭代器：返回适用于 for 的迭代函数
function PersistArrayMap:iter()
    local i = 1
    local arr = self._array
    return function()
        if i <= #arr then
            local key = arr[i]
            local val = arr[i+1]
            i = i + 2
            return key, val
        end
    end
end

-- 键迭代器
function PersistArrayMap:key_iterator()
    local i = 1
    local arr = self._array
    return function()
        if i <= #arr then
            local key = arr[i]
            i = i + 2
            return key
        end
    end
end

-- 值迭代器
function PersistArrayMap:val_iterator()
    local i = 1
    local arr = self._array
    return function()
        if i <= #arr then
            local val = arr[i+1]
            i = i + 2
            return val
        end
    end
end

-- 支持 Lua 的 pairs 元方法
function PersistArrayMap:__pairs()
    return self:iter()
end

-- 归约 (kvreduce)
function PersistArrayMap:kvreduce(f, init)
    local acc = init
    for i = 1, #self._array, 2 do
        acc = f(acc, self._array[i], self._array[i+1])
        -- 如果返回特殊标记 Reduced，这里忽略，因为 Lua 没有 RT
    end
    return acc
end

-- 字符串表示
function PersistArrayMap:__tostring()
    local parts = {}
    for i = 1, #self._array, 2 do
        parts[#parts+1] = ("%s %s"):format(tostring(self._array[i]), tostring(self._array[i+1]))
    end
    return "{" .. table.concat(parts, ", ") .. "}"
end

-- 可编辑集合（asTransient 简化为直接返回表）
function PersistArrayMap:as_transient()
    -- 简单实现：返回可变副本（Lua 中直接暴露数组）
    return {
        array = {table.unpack(self._array)},
        meta  = self._meta,
        len   = #self._array
    }
end

-- 持久化 transient
function PersistArrayMap.persist(transient)
    return PersistArrayMap:new(transient.array, transient.meta)
end

return PersistArrayMap