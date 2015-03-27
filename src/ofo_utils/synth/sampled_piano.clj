(ns ofo-utils.synth.sampled-piano
  (:use [overtone.core]
        [ofo-utils.samples.piano :only [index-buffer rate-buffer]]))

  (definst sampled-piano
    "
sampled piano with microtonal extension (using midi-float note
values) and added length parameter.

    Examples:

    Play sample to the end:

    (sampled-piano 61.5)

    Play sample with specified duration:

    (sampled-piano 61.3 :length 0.1)

    bind a name to a single note:

    (def mynote (sampled-piano 61.5))

    Send it a note-off:

    (ctl mynote :gate 0) 
"
    [note 60 level 1 rate 1 loop? 0
     attack 0 decay 1 sustain 1 release 0.1 curve -4 dur -1 gate 1]
    (let [lengthgate (if (> dur 0) (trig 1 dur) 1)
          buf (index:kr (:id ofo-utils.samples.piano/index-buffer) note)
          env (env-gen (adsr attack decay sustain release level curve)
                       :gate (* gate lengthgate)
                       :action FREE)]
      (* env (scaled-play-buf 2 buf :rate (* rate
                                             (index:kr (:id ofo-utils.samples.piano/rate-buffer) note)
                                             (midiratio (- note (floor note))))
                              :level level :loop loop? :action FREE))))


