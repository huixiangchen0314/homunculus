-- cljh/lang/IFn.lua
local oop = require "cljh.lang.oop"

local M = {}
M.IFn = oop.interface("cljh.lang.IFn")
oop.method(M.IFn, "invoke")

return M