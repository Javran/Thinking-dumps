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

(defn territory-hm [hm coord]
  (loop [owners #{}
         coords #{}
         discovered #{coord}
         q [coord]]
    (if (seq q)
      (let [[cur-coord & rest] q
            next-coords (filter
                         (fn [x]
                           (and
                            (hm x false)
                            (not (discovered x))))
                         (coord-expand cur-coord))
            discovered2 (set/union discovered (set next-coords))]
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
                  discovered2
                  (concat rest next-coords))))
      {:owner 
       (if (and (seq coords)
                (= (count owners) 1))
         (first owners)
         nil)
       :stones coords})))

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
   [" W "]))

