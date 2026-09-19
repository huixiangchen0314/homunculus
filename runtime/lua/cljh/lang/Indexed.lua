-- cljh/lang/Indexed.lua
local oop = require "cljh.lang.oop"
local Counted = require "cljh.lang.Counted".Counted

local M = {}
M.Indexed = oop.interface("cljh.lang.Indexed", Counted)   -- 继承 Counted
oop.method(M.Indexed, "nth")   -- Lua 中用可选参数模拟重载

return M