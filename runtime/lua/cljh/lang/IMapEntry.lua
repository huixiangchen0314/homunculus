-- cljh/lang/IMapEntry.lua
local oop = require "cljh.lang.oop"

local M = {}
M.IMapEntry = oop.interface("cljh.lang.IMapEntry")
oop.method(M.IMapEntry, "key")
oop.method(M.IMapEntry, "val")

return M