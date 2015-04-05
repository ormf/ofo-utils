(ns
    ^{:doc "vumeters for overtone."
      :author "Orm Finnendahl"}
    ofo-utils.gui.vumeter
    (:require [seesaw.core :as ssc]
              [seesaw.graphics :as ssg]
              [seesaw.color :as sscol])
    (:use [overtone.core]
          [overtone.libs event deps]
          [overtone.sc defaults server synth ugens buffer node foundation-groups]
          [overtone.studio core util]
          [overtone.helpers lib]
          [clojure.tools trace])
    (:import [java.awt.event WindowListener ComponentListener]))

(def ^:private highlightcolors
  [(sscol/color :lightblue)
   (sscol/color 0 255 0)
   (sscol/color :yellow)
   (sscol/color :orange)
   (sscol/color :red)
   (sscol/color :purple)])

;; util-functions:

(defn remove-first
  "Returns a lazy sequence of coll with the first occurrence of an
  item for which (pred item) returns true removed. pred must be free
  of side-effects. Not yet implemented for chunked-sequences."
  ([pred coll]
   (lazy-seq
    (when-let [s (seq coll)]
        (let [f (first s) r (rest s)]
          (if (pred f)
            r
            (cons f (remove-first pred r))))))))

;; (remove-first even? '(1 5 2 3 5 3 4))

;; State!

;; a map of bus-ids as keys and their registered vu meter
;; canvases. As there can be more than one canvas per bus-id, the
;; canvases are stored in a sequence.
(defonce vu-bindings* (ref {})) 

;; a map of maps, each map representing a frame, its vumeter
;; canvases and their corresponding buses.
(defonce vu-pool* (ref {}))

;; the state (a list of highlighted db LEDs) of all monitored vu buses.
(defonce vu-states* (ref {})) 

;; a map of all active super collider vumeter-synths. There is only
;; one synth per bus, so the sc bus is used as key.
(defonce vumeter-synths* (ref {})) 

;; The audio group of all supercollider vu-synths:
(defonce vu-group (group "Vu" :tail (foundation-monitor-group)))

(defonce ^:private FPS 10) ;; update rate of vumeter values

;;; led coloring of one vumeter.
(defonce ^:private vu-colors
  (reduce into []
          (map #(repeat %1 (highlightcolors %2))
               [1 15 10 2 11 1] ;; number of LED bars for the
                                ;; different colors, starting with
                                ;; lowest bar.
               [0  1  2 3  4 5] ;; color idx
               )))

;;; db values of the leds of one vumeter.
(defonce ^:private vu-positions
  '(-100 -80 -60 -55 -50 -45 -40 -35 -30 -27 -25 -22 -20 -18 -16 -14 -12 -11 -10 -9 -8 -7 -6 -5 -4 -3 -2 -1 0 1 2 3 4 5 6 8 9 10 11 12))

;; return a list of all leds, which have to get highlighted. Although
;; the function returns a seq of the corresponding db values, only
;; the number of items is relevant for the routine which actually
;; sets the colors of the leds (repaint routine def'd in
;; make-painter).
(defn- get-highlight-vu-positions [db]
  "Given a db value, return a list of all leds, which have to get
  highlighted."
  (take-while #(< % db) vu-positions))

;; the supercollider synth listening on a bus, sending the bus-id and
;; its amp value at regular intervals via OSC. The message is picked
;; up by the event handler defined in add-vumeter-handler.
(defsynth vumeter-synth [in-bus 0, rate FPS] 
  (let [tr (impulse rate)
        val (amplitude (a2k (in in-bus 0.2)) 0.1 0.2)]
    (send-reply tr "/vu" [in-bus val])))

(defn- register-vu
  "add vumeter-canvas to vu-bindings*. Create a sc synth in case the
  bus isn't yet bound to a vumeter."
  [vumeter-canvas bus]
  (dosync
   (if-let
       [bindings (@vu-bindings* bus)]
     (alter vu-bindings* assoc bus (cons vumeter-canvas bindings))
     (do
       (alter vu-states* assoc bus '())
       (alter vumeter-synths* assoc bus (vumeter-synth
                                         [:tail vu-group] bus FPS))
       (alter vu-bindings* assoc bus (list vumeter-canvas))))))

(defn- unregister-vu
  "remove vumeter-canvas from vu-bindings*. Delete the corresponding
  sc synth in case the bus isn't used by (bound to) any other
  vumeter."
  [vumeter-canvas bus]
  (let
      [x (remove-first #(= % vumeter-canvas) (@vu-bindings* bus))]
    (dosync
     (if (empty? x)
       (do
         (alter vu-bindings* dissoc bus)
         (alter vu-states* dissoc bus)
         (try
           (kill (@vumeter-synths* bus))
           (catch Exception e))
         (alter vumeter-synths* dissoc bus))
       (alter vu-bindings* assoc bus x)))))

(defn- vu-set! [bus-id db]
  (dosync (alter vu-states* assoc bus-id
                 (get-highlight-vu-positions db))))

(defn- set-all-vus! [val]
  (if (keys @vu-states*)
    (let [highlighted (get-highlight-vu-positions val)]
      (dosync
       (ref-set vu-states*
                (apply assoc {} (interleave (keys @vu-states*)
                                            (repeat highlighted))))))))

(defn- clear-vus! []
  (set-all-vus! -100))

(defn- make-painter [bus-id]
  "return a repaint function for a vumeter-canvas."
  (fn [c g]
    (ssg/draw g (ssg/rect 0 0 8 120)
          (ssg/style :foreground (sscol/color :black)
                     :background (sscol/color 30 30 30)))
    (doseq [[sscol/color idx] (map list vu-colors (range) (@vu-states* bus-id))]
      (ssg/draw g (ssg/rect 1 (- 118 (* 3 idx)) 6 1)
            (ssg/style :width 1
                   :foreground color
                   :background color)))))

;;; event-handler for vumeters. This gets triggered by the
;;; sc-synths. Each bus monitored by one or more vumeters has one
;;; sc-synth attached to it. The synth sends new values at a fixed
;;; frequency (FPS value, defined in this file), using the "/vu" OSC
;;; message with bus-id and amp as arguments. The event handler sets
;;; the highlight state of the corresponding entry in vu-states* and
;;; triggers repainting.

(defn- add-vumeter-handler []
  (on-event
   "/vu"
   (fn [{:keys [args]}]
     (let [bus (nth args 2)
           amp (nth args 3)]
       (vu-set! (int bus) (+ 10 (amp->db (java.lang.Math/abs amp))))
       (dorun (map #(.repaint %) (@vu-bindings* (int bus))))))
   ::vumeter-evt-handler))

;; (remove-event-handler ::vumeter-evt-handler)

(defn- vumeter-close [id]
  (let [vu-entry (get @vu-pool* id)]
;;    (println (str "closing: " vu-entry))
    (dosync
     (dorun (map unregister-vu (:canvases vu-entry) (:buses vu-entry)))
     (alter vu-pool* dissoc id)))
  (if (empty? @vu-pool*)
    (remove-event-handler ::vumeter-evt-handler)))

(defn- make-vu-frame
  "create a frame containing a canvas for each given bus-id. Return
  the frame and the canvases as map."
  [bus-ids]
  (let [num (count bus-ids)
        vumeter-id (uuid)
        canvases (map #(ssc/canvas ;:id (uuid)
                        :paint (make-painter %))
                      bus-ids)
        frame (ssc/frame :title "VU Meter"
                         :width (* num 8)
                         :height 150
                         :minimum-size [(* num 8) :by 150]
                         :content
                         (ssc/horizontal-panel
                          :background (sscol/color 100 100 100)
                          :items canvases))]
    (.addWindowListener frame
                        (reify WindowListener
                          (windowActivated [this e])
                          (windowClosing [this e]
                            (vumeter-close vumeter-id))
                          (windowDeactivated [this e])
                          (windowDeiconified [this e])
                          (windowIconified [this e])
                          (windowOpened [this e])
                          (windowClosed [this e])))
    {:id vumeter-id :frame frame :canvases canvases :buses bus-ids}))

;;; public api:

(defn vumeter
  "create a new frame with vumeters. 

  Without any arguments the function will create a stereo vumeter
  listening on buses 0 and 1. 

  Given a seq or a vector of numbers as argument, a vu for each item
  in the sequence will be created, the items in the sequence being
  interpreted as bus numbers. Duplicate bus numbers in the sequence or
  in multiple frames are allowed."
  ([] (vumeter [0 1]))
  ([bus-ids]
   (let [frame (make-vu-frame bus-ids)]
     (dosync
      (dorun (map register-vu (:canvases frame) bus-ids))
      (alter vu-pool* assoc (:id frame) frame))
     (add-vumeter-handler) ;; as overtone's event-handler api doesn't
     ;; seem to have a predicate to check
     ;; whether an evt-handler is already
     ;; running, we call it everytime a new
     ;; vumeter is created...
     #_(.setBackground
        (.getContentPane (:frame frame))
        (sscol/color 100 100 100))
     (->
      (:frame frame)
      ssc/pack!
      ssc/show!))))

(defn stop-all-vus []
  (remove-event-handler ::vumeter-evt-handler)
  (dosync
   (dorun (map kill (vals @vumeter-synths*)))
   (ref-set vu-pool* {})
   (ref-set vu-states* {})
   (ref-set vu-bindings* {})
   (ref-set vumeter-synths* {})))

;;; (stop-all-vus)

;;; examples:


(comment
;;; create a stereo vumeter listening on buses 0 and 1:
  (vumeter)
  
;;; create a 16-channnel vumeter listening on buses 0 through 15:
  (vumeter (range 16))

;;; create a 4 channel VUmeter with supercollider's first two input and
;;; output channels:
  (vumeter [8 9 0 1])
  )
