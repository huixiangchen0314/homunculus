-- cljh/lang/Associative.lua
local oop = require "cljh.lang.oop"
local IPersistentCollection = require "cljh.lang.IPersistentCollection".IPersistentCollection
local ILookup = require "cljh.lang.ILookup".ILookup

local M = {}
M.Associative = oop.interface("cljh.lang.Associative", {IPersistentCollection, ILookup})
oop.method(M.Associative, "containsKey")
oop.method(M.Associative, "entryAt")
oop.method(M.Associative, "assoc")

return M