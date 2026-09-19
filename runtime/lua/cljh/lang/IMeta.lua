local oop = require "cljh.lang.oop"

local M = {}
M.IMeta = oop.interface("cljh.lang.IMeta")   -- 全限定名
oop.method(M.IMeta, "meta")

return M