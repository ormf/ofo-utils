(ns ofo-utils.api
  (:import [java.lang.management ManagementFactory])
  (:require clojure.stacktrace
            [ofo-utils.gui vumeter]
            )
  (:use [overtone.live]
        [overtone.helpers.ns]))

(defn immigrate-ofo-utils-api []
  (immigrate 'ofo-utils.gui.vumeter))
