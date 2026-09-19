-- cljh/lang/IHashEq.lua
local oop = require "cljh.lang.oop"

local M = {}
M.IHashEq = oop.interface("cljh.lang.IHashEq")
oop.method(M.IHashEq, "hasheq")

return M