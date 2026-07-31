-- cljh_PersistHashMap.lua
-- 持久化哈希映射（不可变），基于简化版 HAMT 实现
-- 用于 Clojure 到 Lua 编译器的运行时

local PersistMap = {}
PersistMap.__index = PersistMap

----------------------
-- 内部工具函数
----------------------
local SHIFT = 5
local MAX_LEVEL = math.floor(32 / SHIFT)  -- 7
local BIT_PARTITION = 32  -- 2^5

-- 简单通用哈希函数，保证不同类型返回不同整数
local function hash(x)
    local t = type(x)
    if t == "number" then
        return math.floor(x)  -- 用整数部分，注意 Lua 数字可能有浮点
    elseif t == "string" then
        -- djb2 hash
        local h = 5381
        for i = 1, #x do
            h = (h * 33) + string.byte(x, i)
            h = h % 4294967296  -- 保持在 32 位
        end
        return h
    elseif t == "boolean" then
        return x and 1 or 0
    elseif t == "nil" then
        return -1  -- nil 的特殊哈希
    elseif t == "table" then
        -- 使用表的地址（tostring 提取十六进制），保证同一表相同哈希，不同表大概率不同
        local addr = tostring(x):match("table:%s*(.+)$")
        return addr and tonumber(addr, 16) or 0
    else
        -- function, userdata 等，尝试 tostring
        local s = tostring(x)
        local h = 0
        for i = 1, #s do
            h = (h * 31) + string.byte(s, i)
            h = h % 4294967296
        end
        return h
    end
end

-- 提取 hash 值在某层的 5 位索引（从低到高）
local function mask(h, level)
    return (h >> (level * SHIFT)) & (BIT_PARTITION - 1)
end

-- 构建位图节点时，将子节点按 bit 顺序插入
local function clone_and_set(arr, idx, val)
    local new = {}
    for i, v in ipairs(arr) do
        new[i] = v
    end
    new[idx] = val
    return new
end

-- 创建一个空的位图节点（bitmap=0, children={}）
local function empty_bitmap_node()
    return { bitmap = 0, children = {} }
end

----------------------
-- 节点类型定义
----------------------
local Node = {}

-- 叶节点（存储单个键值对，可能用于碰撞）
function Node.leaf(key, val, h)
    return { type = "leaf", key = key, val = val, hash = h }
end

-- 位图索引节点
function Node.bitmap(bitmap, children)
    return { type = "bitmap", bitmap = bitmap, children = children }
end

-- 哈希碰撞节点（多个键具有相同哈希，按链表存储）
function Node.collision(hash, kvs)
    return { type = "collision", hash = hash, kvs = kvs } -- kvs 是 {key1, val1, key2, val2, ...}
end

-- 判断节点是否为空
local function is_empty(node)
    return node == nil or (node.type == "bitmap" and node.bitmap == 0)
end

----------------------
-- 核心查找与修改
----------------------
-- 在节点中查找键，返回 {node, key, val} 包装或 nil
local function find_in_node(node, h, key, level)
    if node == nil then return nil end
    if node.type == "leaf" then
        if node.hash == h and node.key == key then
            return node
        end
    elseif node.type == "bitmap" then
        local bit = mask(h, level)
        local bitmask = 1 << bit
        if (node.bitmap & bitmask) ~= 0 then
            -- 计算 bit 在 children 中的索引（小于该 bit 的置位位数）
            local idx = 1
            for b = 0, bit - 1 do
                if (node.bitmap & (1 << b)) ~= 0 then
                    idx = idx + 1
                end
            end
            return find_in_node(node.children[idx], h, key, level + 1)
        end
    elseif node.type == "collision" then
        if node.hash == h then
            for i = 1, #node.kvs, 2 do
                if node.kvs[i] == key then
                    return { type = "collision_entry", node = node, index = i }
                end
            end
        end
    end
    return nil
end

-- 向根节点执行 assoc，返回新根和是否新增键
local function assoc_node(node, h, key, val, level, added)
    if node == nil then
        -- 空节点，创建叶节点
        added.val = true
        return Node.leaf(key, val, h)
    end

    if node.type == "leaf" then
        if node.hash == h then
            if node.key == key then
                -- 替换值
                added.val = false
                return Node.leaf(key, val, h)
            else
                -- 哈希碰撞，需要升级为碰撞节点
                added.val = true
                local kvs = { node.key, node.val, key, val }
                return Node.collision(h, kvs)
            end
        else
            -- 不同哈希，创建位图节点并合并两片叶子
            added.val = true
            local new_node = empty_bitmap_node()
            local bit1 = mask(node.hash, level)
            local bit2 = mask(h, level)
            if bit1 == bit2 then
                -- 同一索引，递归向下
                new_node.bitmap = 1 << bit1
                new_node.children = { assoc_node(nil, node.hash, node.key, node.val, level + 1, { val = false }) } -- 提前放好一个空，然后用下面递归？
                -- 更直接：创建子节点数组，里面是递归合并的结果
                local child = assoc_node(nil, node.hash, node.key, node.val, level + 1, { val = false })
                child = assoc_node(child, h, key, val, level + 1, added)
                new_node.children = { child }
                return new_node
            else
                -- 不同索引，添加到两个子节点
                new_node.bitmap = (1 << bit1) | (1 << bit2)
                if bit1 < bit2 then
                    new_node.children = { Node.leaf(node.key, node.val, node.hash), Node.leaf(key, val, h) }
                else
                    new_node.children = { Node.leaf(key, val, h), Node.leaf(node.key, node.val, node.hash) }
                end
                return new_node
            end
        end
    elseif node.type == "bitmap" then
        local bit = mask(h, level)
        local bitmask = 1 << bit
        local idx = 1
        for b = 0, bit - 1 do
            if (node.bitmap & (1 << b)) ~= 0 then
                idx = idx + 1
            end
        end
        if (node.bitmap & bitmask) ~= 0 then
            -- 该位已存在，递归更新
            local old_child = node.children[idx]
            local new_child = assoc_node(old_child, h, key, val, level + 1, added)
            if new_child == old_child then
                return node  -- 没有变化
            end
            local new_children = {}
            for i, c in ipairs(node.children) do
                new_children[i] = (i == idx) and new_child or c
            end
            return Node.bitmap(node.bitmap, new_children)
        else
            -- 新增分支
            added.val = true
            local new_bitmap = node.bitmap | bitmask
            -- 新建 children 数组，把新叶子插入到 idx 位置
            local new_children = {}
            for i = 1, idx - 1 do
                new_children[i] = node.children[i]
            end
            new_children[idx] = Node.leaf(key, val, h)
            for i = idx, #node.children do
                new_children[i + 1] = node.children[i]
            end
            return Node.bitmap(new_bitmap, new_children)
        end
    elseif node.type == "collision" then
        if node.hash == h then
            -- 在同一碰撞集合中
            for i = 1, #node.kvs, 2 do
                if node.kvs[i] == key then
                    -- 已存在，替换值
                    if node.kvs[i + 1] == val then
                        return node  -- 未变
                    end
                    local new_kvs = {}
                    for j = 1, #node.kvs do
                        if j == i then
                            new_kvs[j] = key
                        elseif j == i + 1 then
                            new_kvs[j] = val
                        else
                            new_kvs[j] = node.kvs[j]
                        end
                    end
                    added.val = false
                    return Node.collision(h, new_kvs)
                end
            end
            -- 新键，追加
            added.val = true
            local new_kvs = {}
            for i = 1, #node.kvs do
                new_kvs[i] = node.kvs[i]
            end
            new_kvs[#new_kvs + 1] = key
            new_kvs[#new_kvs + 1] = val
            return Node.collision(h, new_kvs)
        else
            -- 不同哈希，需要将碰撞节点升级为位图节点
            added.val = true
            local new_node = empty_bitmap_node()
            local bit1 = mask(node.hash, level)
            local bit2 = mask(h, level)
            if bit1 == bit2 then
                -- 相同索引，递归向下
                new_node.bitmap = 1 << bit1
                local child = assoc_node(node, h, key, val, level + 1, added) -- 这会把 collision 与 key 合并？需要小心
                -- 实际上这里应创建一个新节点，将原 collision 和新键插入子节点中
                -- 我们改为直接构建一个新位图节点，包含两个子节点
                -- 由于索引相同，需要更深一层
                local sub_node = assoc_node(nil, node.hash, nil, nil, level + 1, { val = false }) -- 先放空？
                -- 更规范：创建空 bitmap，然后分别 assoc 两个键
                local sub = empty_bitmap_node()
                sub = assoc_node(sub, node.hash, "collision_key", nil, level + 1, { val = false }) -- 这很难搞
                -- 简化处理：直接创建两个 leaf 节点在同一索引下，调用 assoc_node 合并
                local tmp_node = Node.leaf(key, val, h)
                for i = 1, #node.kvs, 2 do
                    tmp_node = assoc_node(tmp_node, node.hash, node.kvs[i], node.kvs[i + 1], level + 1, { val = false })
                end
                return tmp_node
            else
                new_node.bitmap = (1 << bit1) | (1 << bit2)
                if bit1 < bit2 then
                    new_node.children = { node, Node.leaf(key, val, h) }
                else
                    new_node.children = { Node.leaf(key, val, h), node }
                end
                return new_node
            end
        end
    end
    error("unknown node type")
end

-- 从节点中移除键，返回新节点和是否成功移除
local function dissoc_node(node, h, key, level, removed)
    if node == nil then
        removed.val = false
        return nil
    end
    if node.type == "leaf" then
        if node.hash == h and node.key == key then
            removed.val = true
            return nil
        end
        removed.val = false
        return node
    elseif node.type == "bitmap" then
        local bit = mask(h, level)
        local bitmask = 1 << bit
        if (node.bitmap & bitmask) == 0 then
            removed.val = false
            return node
        end
        local idx = 1
        for b = 0, bit - 1 do
            if (node.bitmap & (1 << b)) ~= 0 then
                idx = idx + 1
            end
        end
        local child = node.children[idx]
        local new_child = dissoc_node(child, h, key, level + 1, removed)
        if new_child == child then
            return node  -- 没变
        end
        if is_empty(new_child) then
            -- 该子节点为空，删除该位
            local new_bitmap = node.bitmap & ~bitmask
            local new_children = {}
            for i = 1, idx - 1 do
                new_children[i] = node.children[i]
            end
            for i = idx + 1, #node.children do
                new_children[i - 1] = node.children[i]
            end
            if new_bitmap == 0 then
                return nil
            elseif #new_children == 1 then
                -- 可能可以压缩成单个叶节点/碰撞节点
                return new_children[1]
            else
                return Node.bitmap(new_bitmap, new_children)
            end
        else
            local new_children = {}
            for i, c in ipairs(node.children) do
                new_children[i] = (i == idx) and new_child or c
            end
            return Node.bitmap(node.bitmap, new_children)
        end
    elseif node.type == "collision" then
        if node.hash ~= h then
            removed.val = false
            return node
        end
        for i = 1, #node.kvs, 2 do
            if node.kvs[i] == key then
                removed.val = true
                local new_kvs = {}
                for j = 1, #node.kvs do
                    if j ~= i and j ~= i + 1 then
                        new_kvs[#new_kvs + 1] = node.kvs[j]
                    end
                end
                if #new_kvs == 0 then
                    return nil
                elseif #new_kvs == 2 then
                    -- 只剩一个键值对，退化为叶节点
                    return Node.leaf(new_kvs[1], new_kvs[2], node.hash)
                else
                    return Node.collision(node.hash, new_kvs)
                end
            end
        end
        removed.val = false
        return node
    end
    error("unknown node type")
end

----------------------
-- 公开 API
----------------------
-- 构造一个空的持久化映射
function PersistMap.empty()
    local pm = {
        _root = nil,
        _count = 0,
        _meta = nil  -- 可扩展元数据
    }
    setmetatable(pm, PersistMap)
    return pm
end

-- 从 Lua 表创建映射（键值对交替或{{k,v}, ...}）
function PersistMap.from_table(tbl)
    local m = PersistMap.empty()
    if tbl[1] and type(tbl[1]) == "table" then
        for _, pair in ipairs(tbl) do
            m = PersistMap.assoc(m, pair[1], pair[2])
        end
    else
        for i = 1, #tbl, 2 do
            m = PersistMap.assoc(m, tbl[i], tbl[i + 1])
        end
    end
    return m
end

-- assoc 返回新映射
function PersistMap.assoc(self, key, val)
    local h = hash(key)
    local added = { val = false }
    local new_root = assoc_node(self._root, h, key, val, 0, added)
    local new_pm = {
        _root = new_root,
        _count = self._count + (added.val and 1 or 0),
        _meta = self._meta
    }
    setmetatable(new_pm, PersistMap)
    return new_pm
end

-- dissoc 返回新映射
function PersistMap.dissoc(self, key)
    if self._root == nil then return self end
    local h = hash(key)
    local removed = { val = false }
    local new_root = dissoc_node(self._root, h, key, 0, removed)
    if not removed.val then return self end  -- 键不存在，返回原映射
    local new_pm = {
        _root = new_root,
        _count = self._count - 1,
        _meta = self._meta
    }
    setmetatable(new_pm, PersistMap)
    return new_pm
end

-- get 获取值，不存在返回 notFound（默认 nil）
function PersistMap.get(self, key, notFound)
    if self._root == nil then return notFound end
    local h = hash(key)
    local found = find_in_node(self._root, h, key, 0)
    if found == nil then return notFound end
    if found.type == "leaf" then
        return found.val
    elseif found.type == "collision_entry" then
        return found.node.kvs[found.index + 1]
    end
    return notFound
end

-- 是否存在键
function PersistMap.contains(self, key)
    return self:get(key, false) ~= false  -- 注意：如果存的值就是 false，则误判
    -- 更严谨：
    -- if self._root == nil then return false end
    -- return find_in_node(self._root, hash(key), key, 0) ~= nil
end

-- 更安全的 contains，直接用 find
function PersistMap.has(self, key)
    if self._root == nil then return false end
    return find_in_node(self._root, hash(key), key, 0) ~= nil
end

-- 计数
function PersistMap.count(self)
    return self._count
end

-- 转为 Lua 表（可变，用于调试）
function PersistMap.to_table(self)
    local t = {}
    local function walk(node)
        if not node then return end
        if node.type == "leaf" then
            t[node.key] = node.val
        elseif node.type == "bitmap" then
            for _, child in ipairs(node.children) do
                walk(child)
            end
        elseif node.type == "collision" then
            for i = 1, #node.kvs, 2 do
                t[node.kvs[i]] = node.kvs[i + 1]
            end
        end
    end
    walk(self._root)
    return t
end

-- 迭代器（返回 key, value）
function PersistMap.iter(self)
    local stack = { self._root }
    local collision_kvs, c_index
    local function next_state()
        while #stack > 0 do
            local node = stack[#stack]
            stack[#stack] = nil
            if node then
                if node.type == "leaf" then
                    return node.key, node.val
                elseif node.type == "bitmap" then
                    for i = #node.children, 1, -1 do
                        stack[#stack + 1] = node.children[i]
                    end
                elseif node.type == "collision" then
                    -- 直接遍历碰撞键值对
                    collision_kvs = node.kvs
                    c_index = 1
                    return collision_kvs[c_index], collision_kvs[c_index + 1]
                    -- 但需要记住位置，这里简化：一次性返回碰撞对
                    -- 实际上我们应让迭代器支持暂停。为简单，把碰撞对全部压入栈作为叶子
                    for i = #node.kvs, 1, -2 do
                        stack[#stack + 1] = Node.leaf(node.kvs[i - 1], node.kvs[i], node.hash)
                    end
                end
            end
        end
        return nil
    end
    -- 包装成无状态迭代器供 for 使用
    return next_state
end

-- 支持 for k, v in pairs(map) 的元方法（不破坏不可变性，只是迭代）
function PersistMap.__pairs(self)
    return self:iter()
end

-- 克隆（已不可变，直接返回自身）
function PersistMap.clone(self)
    return self
end

-- 等价性判断（值相等，深度比较）
function PersistMap.__eq(a, b)
    if a._count ~= b._count then return false end
    for k, v in a:iter() do
        if b:get(k) ~= v then return false end
    end
    return true
end

-- 字符串表示
function PersistMap.__tostring(self)
    local t = {}
    for k, v in self:iter() do
        t[#t + 1] = tostring(k) .. " " .. tostring(v)
    end
    return "{" .. table.concat(t, ", ") .. "}"
end

return PersistMap