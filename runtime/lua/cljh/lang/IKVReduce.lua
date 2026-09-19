-- cljh/lang/IKVReduce.lua
local oop = require "cljh.lang.oop"

local M = {}
M.IKVReduce = oop.interface("cljh.lang.IKVReduce")   -- 全限定名，cljh.lang 前缀
oop.method(M.IKVReduce, "kvreduce")

return M