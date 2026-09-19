-- cljh/lang/IDeref.lua
local oop = require "cljh.lang.oop"

local M = {}
M.IDeref = oop.interface("cljh.lang.IDeref")
oop.method(M.IDeref, "deref")

return M