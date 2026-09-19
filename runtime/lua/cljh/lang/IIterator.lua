-- cljh.lang.IIterator.lua
local oop = require "cljh.lang.oop"

local M = {}
M.IIterator = oop.interface("cljh.lang.IIterator")   -- 全限定名
oop.method(M.IIterator, "hasNext")
oop.method(M.IIterator, "next")

return M