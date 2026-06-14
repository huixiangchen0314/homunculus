(ns top.kzre.homunculus.cli.core
  "CLI 入口：使用 tools.cli 解析命令行参数，返回编译配置 map。"
  (:require [clojure.spec.alpha :as s]
            [top.kzre.homunculus.cli.spec :as spec])
  (:gen-class))
