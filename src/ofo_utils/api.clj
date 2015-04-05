(ns ofo-utils.api
  (:import [java.lang.management ManagementFactory])
  (:require clojure.stacktrace
            [ofo-utils.gui vumeter]
;            [ofo-utils.synth sampled-piano]
            )
  (:use [overtone.core]
        [overtone.helpers.ns]))

(defn immigrate-ofo-utils-api []
  (immigrate 'overtone.live
             'ofo-utils.gui.vumeter
;;             'ofo-utils.synth.sampled-piano
             ))
