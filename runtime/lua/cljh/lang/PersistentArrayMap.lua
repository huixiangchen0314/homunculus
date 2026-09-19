local oop = require "cljh.lang.oop"

local PersistArrayMap = oop.class("cljh.lang.PersistentArrayMap")

-- 常量
PersistArrayMap.HASHTABLE_THRESHOLD = 16
PersistArrayMap.EMPTY = nil

-- 构造函数初始化：设置 _array 和 _meta
function PersistArrayMap:_cljh_init(array, meta)
    self._array = array or {}
    self._meta = meta
end

-- 静态方法：from_table
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
    return PersistArrayMap(arr)  -- 使用 __call
end

-- 静态方法：create_with_check
function PersistArrayMap.create_with_check(init)
    for i = 1, #init, 2 do
        for j = i + 2, #init, 2 do
            if PersistArrayMap.equal_key(init[i], init[j]) then
                error("Duplicate key: " .. tostring(init[i]))
            end
        end
    end
    return PersistArrayMap(init)
end

-- 静态方法：create_as_if_by_assoc
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
        return PersistArrayMap(init)
    end
end

function PersistArrayMap._create_as_if_by_assoc_complex(init, has_trailing)
    if has_trailing then
        local trailing = init[#init]
        local new_init = {}
        for i = 1, #init - 1 do
            new_init[i] = init[i]
        end
        if type(trailing) == "table" and trailing[1] and type(trailing[1]) == "table" then
            for _, pair in ipairs(trailing) do
                new_init[#new_init+1] = pair[1]
                new_init[#new_init+1] = pair[2]
            end
        end
        init = new_init
    end

    local nodups = {}
    local seen = {}
    for i = 1, #init, 2 do
        seen[init[i]] = i
    end
    local m = 0
    for i = 1, #init, 2 do
        if seen[init[i]] == i then
            nodups[m+1] = init[i]
            nodups[m+2] = init[i+1]
            m = m + 2
        end
    end
    return PersistArrayMap(nodups)
end

-- 内部工具
function PersistArrayMap.indexOf(self, key)
    local array = self._array
    for i = 1, #array, 2 do
        if key == array[i] then
            return i
        end
    end
    return 0
end

function PersistArrayMap.equal_key(k1, k2)
    return k1 == k2
end

function PersistArrayMap:needs_upgrade()
    return #self._array >= PersistArrayMap.HASHTABLE_THRESHOLD
end

-- 升级工厂
PersistArrayMap.createHT_factory = nil

function PersistArrayMap:createHT(array)
    if PersistArrayMap.createHT_factory then
        return PersistArrayMap.createHT_factory(self._meta, array)
    else
        return PersistArrayMap(array, self._meta)
    end
end

-- 核心接口
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

function PersistArrayMap:assoc(key, val)
    local i = self:indexOf(key)
    local array = self._array
    if i > 0 then
        if array[i+1] == val then
            return self
        end
        local new_array = {}
        for j = 1, #array do
            if j == i+1 then
                new_array[j] = val
            else
                new_array[j] = array[j]
            end
        end
        return PersistArrayMap(new_array, self._meta)
    else
        if #array >= PersistArrayMap.HASHTABLE_THRESHOLD then
            local ht = self:createHT(array)
            return ht:assoc(key, val)
        end
        local new_array = {}
        for j = 1, #array do
            new_array[j] = array[j]
        end
        new_array[#new_array+1] = key
        new_array[#new_array+1] = val
        return PersistArrayMap(new_array, self._meta)
    end
end

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
    new_array[1] = key
    new_array[2] = val
    for j = 1, #self._array do
        new_array[j+2] = self._array[j]
    end
    return PersistArrayMap(new_array, self._meta)
end

function PersistArrayMap:without(key)
    local i = self:indexOf(key)
    if i > 0 then
        local new_len = #self._array - 2
        if new_len == 0 then
            return self:empty()
        end
        local new_array = {}
        for j = 1, i-1 do
            new_array[j] = self._array[j]
        end
        for j = i+2, #self._array do
            new_array[j-2] = self._array[j]
        end
        return PersistArrayMap(new_array, self._meta)
    else
        return self
    end
end

function PersistArrayMap:empty()
    if self._meta then
        return PersistArrayMap.EMPTY:with_meta(self._meta)
    else
        return PersistArrayMap.EMPTY
    end
end

function PersistArrayMap:meta()
    return self._meta
end

function PersistArrayMap:with_meta(meta)
    if self._meta == meta then
        return self
    end
    return PersistArrayMap(self._array, meta)
end

-- 序列相关
local Seq = oop.class("PersistentArrayMap.Seq")

function Seq:_cljh_init(array, index)
    self.array = array
    self.index = index
end

function Seq:first()
    return { key = self.array[self.index], val = self.array[self.index+1] }
end

function Seq:next()
    if self.index + 2 <= #self.array then
        return Seq(self.array, self.index + 2)
    else
        return nil
    end
end

function Seq:count()
    return math.floor((#self.array - self.index + 1) / 2)
end

PersistArrayMap.Seq = Seq

function PersistArrayMap:seq()
    if #self._array == 0 then
        return nil
    end
    return Seq(self._array, 1)
end

-- 迭代器
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

function PersistArrayMap:__pairs()
    return self:iter()
end

function PersistArrayMap:kvreduce(f, init)
    local acc = init
    for i = 1, #self._array, 2 do
        acc = f(acc, self._array[i], self._array[i+1])
    end
    return acc
end

function PersistArrayMap:__tostring()
    local parts = {}
    for i = 1, #self._array, 2 do
        parts[#parts+1] = ("%s %s"):format(tostring(self._array[i]), tostring(self._array[i+1]))
    end
    return "{" .. table.concat(parts, ", ") .. "}"
end

function PersistArrayMap:as_transient()
    return {
        array = {table.unpack(self._array)},
        meta = self._meta,
        len = #self._array
    }
end

function PersistArrayMap.persist(transient)
    return PersistArrayMap(transient.array, transient.meta)
end

-- 初始化空实例
PersistArrayMap.EMPTY = PersistArrayMap({})

return PersistArrayMap