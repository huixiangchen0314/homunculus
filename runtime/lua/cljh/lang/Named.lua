-- cljh/lang/Named.lua
local oop = require "cljh.lang.oop"

local M = {}
M.Named = oop.interface("cljh.lang.Named")
oop.method(M.Named, "getNamespace")
oop.method(M.Named, "getName")

return M