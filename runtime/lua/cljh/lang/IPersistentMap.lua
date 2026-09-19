-- cljh/lang/IPersistentMap.lua
local oop = require "cljh.lang.oop"
local Iterable = require "cljh.lang.Iterable".Iterable
local Associative = require "cljh.lang.Associative".Associative
local Counted = require "cljh.lang.Counted".Counted

local M = {}
M.IPersistentMap = oop.interface("cljh.lang.IPersistentMap", {Iterable, Associative, Counted})
oop.method(M.IPersistentMap, "assoc")
oop.method(M.IPersistentMap, "assocEx")
oop.method(M.IPersistentMap, "without")

return M