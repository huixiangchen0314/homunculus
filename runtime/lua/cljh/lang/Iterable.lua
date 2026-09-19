-- cljh/lang/Iterable.lua
local oop = require "cljh.lang.oop"
local M = {}
M.Iterable = oop.interface("cljh.lang.Iterable")
oop.method(M.Iterable, "iterator")
return M