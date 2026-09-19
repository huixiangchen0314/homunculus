-- cljh/lang/Seqable.lua
local oop = require "cljh.lang.oop"

local M = {}
M.Seqable = oop.interface("cljh.lang.Seqable")
oop.method(M.Seqable, "seq")

return M