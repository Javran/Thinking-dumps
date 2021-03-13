(ns go-counting
  (:require [clojure.set :as set]))

(defn coord-expand [[x y]]
  [[(- x 1) y]
   [(+ x 1) y]
   [x (- y 1)]
   [x (+ y 1)]])

(defn mapcat-indexed [f xs]
  (apply concat (map-indexed f xs)))

(defn grid->hash-map [grid]
  (apply
   hash-map
   (mapcat-indexed
    (fn [j xs]
      (mapcat-indexed
       (fn [i x] [[i j] x]) xs))
    grid)))

(defn territory-hm
  "Collects set of neighboring stone coords and the owner"
  [hm coord]
  (loop [;; keeps track potential owners (non-space cells near current set of coords
         owners #{}
         ;; set of space coords
         coords #{}
         ;; below are standard BFS stuff
         discovered #{coord}
         q (conj (clojure.lang.PersistentQueue/EMPTY) coord)]
    (let [cur-coord (peek q)]
      (if cur-coord
        (let [rest (pop q)
              next-coords (filter
                           (fn [x]
                             (and
                              (hm x false)
                              (not (discovered x))))
                           (coord-expand cur-coord))]
          (case (hm cur-coord)
            \B (recur
                (conj owners :black)
                coords
                discovered
                rest)
            \W (recur
                (conj owners :white)
                coords
                discovered
                rest)
            \space (recur
                    owners
                    (conj coords cur-coord)
                    (set/union discovered (set next-coords))
                    (apply (partial conj rest) next-coords))))
        {:owner
         (if (and (seq coords)
                  (= (count owners) 1))
           (first owners)
           nil)
         :stones coords}))))

(defn territory [grid coord]
  (territory-hm (grid->hash-map grid) coord))

(defn territories [grid]
  (let [hm (grid->hash-map grid)]
    (loop [occupied #{}
           ans {:black-territory #{}
                :white-territory #{}
                :null-territory #{}}
           xs (keys hm)]
      (if (empty? xs)
        ans
        (let [[cur & rest] xs]
          (if (occupied cur)
            (recur occupied ans rest)
            (case (hm cur)
              \space
              (let [{owner :owner, stones :stones} (territory-hm hm cur)
                    key (case owner
                          nil :null-territory
                          :black :black-territory
                          :white :white-territory)]
                (recur (set/union occupied stones)
                       (update ans key (partial set/union stones))
                       rest))
              (recur occupied ans rest))))))))

(def e
  (territories
   ["  B  "
    " B B "
    "B W B"
    " W W "
    "  W  "
    "  B  "]))
