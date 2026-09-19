-- cljh/lang/IMapIterable.lua
local oop = require "cljh.lang.oop"

local M = {}
M.IMapIterable = oop.interface("cljh.lang.IMapIterable")   -- 全限定名使用 cljh.lang 前缀
oop.method(M.IMapIterable, "keyIterator")
oop.method(M.IMapIterable, "valIterator")

return M