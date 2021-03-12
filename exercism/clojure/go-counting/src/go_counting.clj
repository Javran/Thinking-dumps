(ns go-counting
  (:use [clojure.set])
  )

(defn coord-expand [[x y]]
  [[(- x 1) y]
   [(+ x 1) y]
   [x (- y 1)]
   [x (+ y 1)]])

(defn grid->hash-map [grid]
  (apply 
   hash-map
   (apply 
    concat
    (map-indexed 
     (fn [j xs]
       (apply 
        concat
        (map-indexed
         (fn [i x] [[i j] x]) xs)))
     grid))))


(defn territory-hm [hm coord]
  (defn floodfill [owners coords discovered q]
      (if (not (empty? q))
        (do
          (let [[cur-coord & rest] q
              next-coords (filter
                           (fn [x]
                             (and
                              (hm x false)
                              (not (discovered x))))
                           (coord-expand cur-coord))
              discovered2 (union discovered (set next-coords))]
         (case (hm cur-coord)
           \B (floodfill
                (conj owners :black)
                coords
                discovered
                rest)
           \W (floodfill
                (conj owners :white)
                coords
                discovered
                rest)
           \space (floodfill
                    owners
                    (conj coords cur-coord)
                    discovered2
                    (concat rest next-coords)))))
        {:owner 
         (if (and (not (empty? coords))
                  (= (count owners) 1))
           (first owners)
           nil)
         :stones coords}))
    (floodfill #{} #{} #{coord} [coord]))

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
                (recur (union occupied stones)
                       (update ans key (partial union stones))
                       rest))
              (recur occupied ans rest))))))))

(def e
  (territories
   [" W "]))

