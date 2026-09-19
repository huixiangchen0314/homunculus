-- cljh/lang/IPersistentStack.lua
local oop = require "cljh.lang.oop"
local IPersistentCollection = require "cljh.lang.IPersistentCollection".IPersistentCollection

local M = {}
M.IPersistentStack = oop.interface("cljh.lang.IPersistentStack", IPersistentCollection)
oop.method(M.IPersistentStack, "peek")
oop.method(M.IPersistentStack, "pop")

return M