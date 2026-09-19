-- cljh/lang/IPersistentCollection.lua
local oop = require "cljh.lang.oop"
local Seqable = require "cljh.lang.Seqable".Seqable

local M = {}
M.IPersistentCollection = oop.interface("cljh.lang.IPersistentCollection", Seqable)
oop.method(M.IPersistentCollection, "count")
oop.method(M.IPersistentCollection, "cons")
oop.method(M.IPersistentCollection, "empty")
oop.method(M.IPersistentCollection, "equiv")

return M