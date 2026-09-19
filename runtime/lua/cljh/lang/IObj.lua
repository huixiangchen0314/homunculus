-- cljh/lang/IObj.lua
local oop = require "cljh.lang.oop"
local IMetaModule = require "cljh.lang.IMeta"
local IMeta = IMetaModule.IMeta

local M = {}
M.IObj = oop.interface("cljh.lang.IObj", IMeta)   -- 全限定名，继承 IMeta
oop.method(M.IObj, "withMeta")

return M