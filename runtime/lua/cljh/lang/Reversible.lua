-- cljh/lang/Reversible.lua
local oop = require "cljh.lang.oop"

local M = {}
M.Reversible = oop.interface("cljh.lang.Reversible")
oop.method(M.Reversible, "rseq")   -- 返回 ISeq

return M