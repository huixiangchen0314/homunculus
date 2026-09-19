-- cljh/lang/IPersistentVector.lua
local oop = require "cljh.lang.oop"
local Associative = require "cljh.lang.Associative".Associative
local Sequential = require "cljh.lang.Sequential".Sequential
local IPersistentStack = require "cljh.lang.IPersistentStack".IPersistentStack
local Reversible = require "cljh.lang.Reversible".Reversible
local Indexed = require "cljh.lang.Indexed".Indexed

local M = {}
M.IPersistentVector = oop.interface("cljh.lang.IPersistentVector",
        {Associative, Sequential, IPersistentStack, Reversible, Indexed})
oop.method(M.IPersistentVector, "length")
oop.method(M.IPersistentVector, "assocN")
oop.method(M.IPersistentVector, "cons")

return M