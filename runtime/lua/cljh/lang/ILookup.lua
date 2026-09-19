-- cljh/lang/ILookup.lua
local oop = require "cljh.lang.oop"

local M = {}
M.ILookup = oop.interface("cljh.lang.ILookup")
oop.method(M.ILookup, "valAt")        -- 注意：Lua 不支持重载，用可选参数模拟
-- 第二个方法 valAt(Object, Object) 通过同一函数实现，根据参数个数区分

return M