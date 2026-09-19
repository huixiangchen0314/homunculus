-- cljh/lang/ISeq.lua
local oop = require "cljh.lang.oop"
local IPersistentCollection = require "cljh.lang.IPersistentCollection".IPersistentCollection

local M = {}
M.ISeq = oop.interface("cljh.lang.ISeq", IPersistentCollection)
oop.method(M.ISeq, "first")
oop.method(M.ISeq, "next")
oop.method(M.ISeq, "more")
oop.method(M.ISeq, "cons")   -- 注意：IPersistentCollection 已有 cons，这里继承关系会自动保留

return M