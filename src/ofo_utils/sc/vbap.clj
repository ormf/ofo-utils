(ns ofo-utils.sc.vbap
  )


;;; vector utility functions

(do
  (defn sqr [n] (* n n))
  (defn sqrt [n] (Math/sqrt n))
  (defn abs [n] (Math/abs n))
  (defn acos [n] (Math/acos n))
  (defn cos [n] (Math/cos n))
  (defn sin [n] (Math/sin n))
  (def v+ (partial mapv +))
  (def v- (partial mapv -))
  (def v= (comp (partial every? true?) (partial map ==)))
  (defn v-len-sqr [u] (reduce + (map sqr u)))
  (def v-len (comp sqrt v-len-sqr))

  (defn v* [u v]
    (cond
      (and (vector? u) (vector? v)) (mapv * u v)
      (vector? u) (mapv (partial * v) u)
      (vector? v) (mapv (partial * u) v)))

  (defn v-neg [u] (v* -1 u))
  (def v-dist (comp v-len v-))
  (def v-dist-sqr (comp v-len-sqr v-))

  (defn v-setlen [u t]
    (let [dim (count u)]
      (if (every? (partial == 0) u)     ; [0 0 ...]?
        (into [t] (repeat (dec dim) 0)) ; [t 0 ...]
        (v* u (/ t (v-len u))))))

  (defn v-norm [u]
    (v-setlen u 1))

  (defn v-dot [u v]
    (reduce + (map * u v)))

  (defn v-cross-prod [[x1 y1 z1] [x2 y2 z2]]
    (v- (v* [y1 z1 x1] [z2 x2 y2])
        (v* [z1 x1 y1] [y2 z2 x2])))

  (defn v-unq-cross-prod [u v]
    (v-norm (v-cross-prod u v)))

  (defn norm-clip
    "clip n to range [-1..1]. Returns a float."
    [n]
    (* (Math/signum (float n)) (min 1 (abs n))))

  (defn v-angle
    "return angle between u and v (any dimension) in radians."
    [u v]
    (-> (/ (v-dot u v)
           (* (v-len u) (v-len v)))
        norm-clip acos abs))

  (defn v-perm-prod-diff [[a1 a2 a3] [b1 b2 b3]]
    (v- (v* [a2 a3 a1] [b3 b1 b2])
        (v* [b2 b3 b1] [a3 a1 a2])))

  (defn inv-det [x y z]
    (/ (reduce + (v* x (v-perm-prod-diff y z)))))

  (defn mtx-inverse [[x y z]]
    (mapv #(v* (inv-det x y z)
               (v-perm-prod-diff %1 %2)) [y z x] [z x y]))

  (def any? (comp boolean some))

  (defn ls-outside? [ls inv-matrix]
    (any? (map #(< (v-dot ls %) -0.001) inv-matrix)))

  (defn no-ls-inside-triplet?
    [triplet speakers]
    (let [inv-mtx (mtx-inverse triplet)]
      (every? (map #(ls-outside? % inv-mtx) speakers))))

  ;; ordered vector version, not tail-recursive, not lazy

  (defn all-combinations-vector [l n]
    (cond (= n 1) (into [] (map vector l))
          (empty? l) []
          :else (into
                 (into [] (map #(into (vector (first l)) %)
                               (all-combinations-vector (rest l) (dec n))))
                 (all-combinations-vector (rest l) n))))

  (defn all-combinations-set [l n]
    (cond (= n 1) (into [] (map #(conj #{} %) l))
          (empty? l) []
          :else (into
                 (all-combinations-set (rest l) n)
                 (into [] (map #(conj % (first l))
                             (all-combinations-set (rest l) (dec n)))))))

  (defn ang->cart
    [azi ele]
    (let [atorad (* Math/PI 2/360)]
      [(* (Math/cos (* azi atorad)) (Math/cos (* ele atorad)))
       (* (Math/sin (* azi atorad)) (Math/cos (* ele atorad)))
       (Math/sin (* ele atorad))]))

  (defn init-speaker
    ([idx dir]
     (if (number? dir) (init-speaker idx (list dir 0))
         (let [[azi ele] dir]
           {:azi azi :ele ele :coords (ang->cart azi ele) :chan-offset idx}))))

  (defn vol-p-side-lgth [i j k]
    (let [volper (abs (v* (v-unq-cross-prod i j) k))
          lgth (reduce + (map #(abs (v-angle %1 %2))
                              ((i j) (i k) (j k))))]
      (if (> lgth 0.00001) (/ volper lgth) 0)))

  (defn lines-intersect?
    "check if lines i j and k l intersect on the unit sphere."
    [[i j] [k l]]
    (let [v3 (v-unq-cross-prod ;;; crossing point of planes ij and kl on unit sphere
              (v-unq-cross-prod i j)
              (v-unq-cross-prod k l))
          nv3 (v-neg v3) ;;; crossing point on opposite side of unit sphere
          d-ij (v-angle i j), d-kl (v-angle k l) ;;; distances between points
          d-iv3 (v-angle i v3), d-jv3 (v-angle j v3)
          d-kv3 (v-angle k v3), d-lv3 (v-angle l v3)
          d-inv3 (v-angle i nv3), d-jnv3 (v-angle j nv3)
          d-knv3 (v-angle k nv3), d-lnv3 (v-angle l nv3)]
      (and
;;; no speaker close to crossing points
       (every? #(> (abs %) 0.01)
               [d-iv3 d-jv3 d-kv3 d-lv3 d-inv3 d-jnv3 d-knv3 d-lnv3])
;;; crossing point is on lines between both speaker pairs
       (or (and (<= (abs (- d-ij (+ d-iv3 d-jv3))) 0.01)
                (<= (abs (- d-kl (+ d-kv3 d-lv3))) 0.01))
           (and (<= (abs (- d-ij (+ d-iv3 d-jnv3))) 0.01)
                (<= (abs (- d-kl (+ d-kv3 d-lnv3))) 0.01))))))



  (defn vbap-speaker-array [dim directions]
    (map-indexed init-speaker directions)))

#_(seq (all-combinations-vector (range 8) 2))

#_(filter #(no-ls-inside-triplet? % speakers) triplets)

#_(mtx-inverse (mtx-inverse [[1 2.4 -1] [2 1.7 3] [-1 2.3 1]]))

#_(map #(v-len (:coords %)) (vbap-speaker-array 2 '(-45 0 45 90 135 180 -135 -90)))

#_(vbap-speaker-array 3 (map #(list % 45) '(-45 0 45 90 135 180 -135 -90)))

(defn set-all-connections [speakers]
  (map  #(assoc % :connected-to (filter (fn [sp] (not (= sp %))) speakers))
        speakers))

(def ^:private speakers (vbap-speaker-array 3 (map #(list % 45) '(-45 0 45))))


(defn get-all-connections
  "return all possible speaker connections as sets, sorted by their distance."
  [speakers]
  (map :speakers ;;; we only need the sorted speaker pairs
       (sort #(< (:dist %1) (:dist %2)) ;; sort pairs by distance
;;; collect maps of all possible speaker pairs and their distances
             (map #(assoc {} :speakers %, :dist (apply v-angle (map :coords %)))
                  (all-combinations-set speakers 2)))))

(defn remove-intersecting-pairs [connections]
  (cond (empty? connections) '()
        :else (cons (first connections)
                    (remove-intersecting-pairs
                     (filter #(not (lines-intersect?
                                    (into [] (map :coords (first connections)))
                                    (into [] (map :coords %))))
                             (rest connections))))))

(defn triplet-connectable? [triplet connections]
  (every? (fn[conn] (any? #(= % conn) connections))
          (all-combinations-set triplet 2)))

(triplet-connectable? #{1 2 3} '(#{1 4} #{1 3}
                            #{2 3}
                            #{2 1}
                            #{1 5}))

(remove-intersecting-pairs
 (get-all-connections speakers)
 )

  (v-angle (v-norm [-1 1 1]) (v-norm [1 1 1]))
  (v-angle (v-norm [1 -1 1]) (v-norm [1 2 1]))
(lines-intersect?
 [(v-norm [-1 1 1])(v-norm [1 1 1])]
 [(v-norm [0 -1 0])(v-norm [0 2 1])])

(lines-intersect?
 [[0.7071067811865476 0.0 0.7071067811865475] [0.5000000000000001 0.5 0.7071067811865475]]
 [[0.7071067811865476 0.0 0.7071067811865475] [0.5000000000000001 -0.5 0.7071067811865475]])

(every? #(> % 1) [3 4 5])

(lines-intersect
 (into [] (map :coords (first (get-all-connections speakers))))
 (into [] (map :coords (second (get-all-connections speakers)))))

(remove-intersecting-pairs
 (get-all-connections speakers)
 )

(#{{:azi 0, :ele 45, :coords [0.7071067811865476 0.0 0.7071067811865475], :chan-offset 1} {:azi 45, :ele 45, :coords [0.5000000000000001 0.5 0.7071067811865475], :chan-offset 2}} #{{:azi 0, :ele 45, :coords [0.7071067811865476 0.0 0.7071067811865475], :chan-offset 1} {:azi -45, :ele 45, :coords [0.5000000000000001 -0.5 0.7071067811865475], :chan-offset 0}} #{{:azi 45, :ele 45, :coords [0.5000000000000001 0.5 0.7071067811865475], :chan-offset 2} {:azi -45, :ele 45, :coords [0.5000000000000001 -0.5 0.7071067811865475], :chan-offset 0}})

(#({:speakers % :distance (apply v-angle (map :coords %))})
 #{{:azi 0, :ele 45, :coords [0.7071067811865476 0.0 0.7071067811865475], :chan-offset 1} {:azi 45, :ele 45, :coords [0.5000000000000001 0.5 0.7071067811865475], :chan-offset 2}})

(first (all-combinations-set speakers 2))

(map :coords (first (all-combinations-set speakers 2)))
(= (into #{}'(3 1 2 4)) (into #{}'(1 2 3 4)))


(set-all-connections (vbap-speaker-array 3 (map #(list % 45) '(-45 0 45))))

(not (= 4 4))

(all-combinations-vector
 (vbap-speaker-array 3 (map #(list % 45) '(-45 0 45 90 135 180 -135 -90)))
 3)

#_(vbap-speaker-array 2 '(-45 0 45 90 135 180 -135 -90))


;; unordered list version, non tail-recursive (shorter, but slower!)

#_(defn all-combinations-list [l n]
  (cond (= n 1) (map list l)
        (empty? l) '()
        :else (into
               (map #(cons (first l) %)
                    (all-combinations-list (rest l) (dec n)))
               (all-combinations-list (rest l) n))))

#_(map #(into [] %) (all-combinations-list (range 5) 3))

(defn calc-3d-triplet-map
  [triplet]
  {:triplet triplet
   :inv-mtx (mtx-inverse triplet)})

(defn make-3d-buf [triplets]
  (map #()))

{:coords
 :chan-offset}

(#(into (into [] (map :chan-offset %))
        (into (reduce into (mtx-inverse (map :coords %)))
              (reduce into (map :coords %))))
 [{:coords [1 0 0] :chan-offset 0}
  {:coords [0 1 0] :chan-offset 1}
  {:coords [0 0 1] :chan-offset 2}])

(mtx-inverse [[1 0 2] [0 1 0] [0 0 1]])

 calculate_3x3_matrixes {
                                 /* Calculates the inverse matrices for 3D */

                                 var invdet                   ;
                                 var lp1, lp2, lp3            ;
                                 var invmx                    ;
                                 var triplet_amount = 0, pointer,list_length=0 ;
                                 var result ;

                                 if(sets.isNil, {
                                                 postln("define-loudspeakers: Not valid 3-D configuration") ;
                                                 ^nil ;
                                                 })   ;

                                 triplet_amount = sets.size   ;
                                 //"triplet_amount: %\n".postf(triplet_amount) ;
                                 list_length = triplet_amount * 21 + 2 ;
                                 result = FloatArray.newClear(list_length) ;

                                 result[0] = dim ;
                                 result[1] = numSpeakers ;
                                 pointer=2               ;

                                 sets.do({|set|
                                          lp1 = speakers[set.chanOffsets[0]] ;
                                          lp2 = speakers[set.chanOffsets[1]] ;
                                          lp3 = speakers[set.chanOffsets[2]] ;

                                          invmx = FloatArray.newClear(9) ;

                                          //"lp1x: % lp1y: % lp1z: %\n".postf(lp1.x, lp1.y, lp1.z) ;
                                          //"lp2x: % lp2y: % lp2z: %\n".postf(lp2.x, lp2.y, lp2.z) ;
                                          //"lp3x: % lp3y: % lp3z: %\n".postf(lp3.x, lp3.y, lp3.z) ;
                                          invdet = 1.0 / (  (lp1.x * ((lp2.y * lp3.z) - (lp2.z * lp3.y)))
                                                            - (lp1.y * ((lp2.x * lp3.z) - (lp2.z * lp3.x)))
                                                            + (lp1.z * ((lp2.x * lp3.y) - (lp2.y * lp3.x)))) ;

                                          //"invdet: %\n".postf(invdet) ;

                                          invmx[0] = ((lp2.y * lp3.z) - (lp2.z * lp3.y)) * invdet ;
                                          invmx[3] = ((lp1.y * lp3.z) - (lp1.z * lp3.y)) * invdet.neg ;
                                          invmx[6] = ((lp1.y * lp2.z) - (lp1.z * lp2.y)) * invdet ;
                                          invmx[1] = ((lp2.x * lp3.z) - (lp2.z * lp3.x)) * invdet.neg ;
                                          invmx[4] = ((lp1.x * lp3.z) - (lp1.z * lp3.x)) * invdet ;
                                          invmx[7] = ((lp1.x * lp2.z) - (lp1.z * lp2.x)) * invdet.neg ;
                                          invmx[2] = ((lp2.x * lp3.y) - (lp2.y * lp3.x)) * invdet ;
                                          invmx[5] = ((lp1.x * lp3.y) - (lp1.y * lp3.x)) * invdet.neg ;
                                          invmx[8] = ((lp1.x * lp2.y) - (lp1.y * lp2.x)) * invdet ;
                                          set.inv_mx = invmx ;
                                          3.do({|i|
                                                result[pointer] = set.chanOffsets[i] + 1 ;
                                                pointer = pointer + 1 ;
                                                }) ;
                                          9.do({|i|
                                                result[pointer] = invmx[i] ;
                                                pointer = pointer + 1 ;
                                                }) ;
                                          result[pointer] = lp1.x ; pointer = pointer + 1;
                                          result[pointer] = lp2.x ; pointer = pointer + 1;
                                          result[pointer] = lp3.x ; pointer = pointer + 1;
                                          result[pointer] = lp1.y ; pointer = pointer + 1;
                                          result[pointer] = lp2.y ; pointer = pointer + 1;
                                          result[pointer] = lp3.y ; pointer = pointer + 1;
                                          result[pointer] = lp1.z ; pointer = pointer + 1;
                                          result[pointer] = lp2.z ; pointer = pointer + 1;
                                          result[pointer] = lp3.z ; pointer = pointer + 1;

                                          }) ;
                                 ^result     ;
                                 }


