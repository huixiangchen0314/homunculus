-- cljh/lang/Counted.lua
local oop = require "cljh.lang.oop"
local M = {}
M.Counted = oop.interface("cljh.lang.Counted")
oop.method(M.Counted, "count")
return M