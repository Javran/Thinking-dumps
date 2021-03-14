(ns pov)

;; a context is:
;; {
;;   :parent-tag <tag of its parent>
;;   :lefts <chilren to the left it>
;;   :rights <chilren to the right it>
;; }
;;
;; Note: merging lefts and rights won't have correctness implications
;; for the purpose of this exercise.
;; but having this distinction is necessary to reconstruct the original tree.

;; a zipper is:
;; {
;;   :focus <current focusing tree>
;;   :contexts <a stack of contexts>
;; }

(defn build-zippers
  "Returns a seq of zippers for every node of the given tree."
  [tree contexts]
  (let [[cur-tag & children] tree
        children-contexts
        (map (fn [i]
               (let [[xs rights] (split-at (+ i 1) children)
                     lefts (butlast xs)]
                 (cons
                  {:parent-tag cur-tag
                   :lefts lefts
                   :rights rights}
                  contexts)))
             (range 0 (count children)))]
    (cons {:focus tree :contexts contexts}
          (mapcat build-zippers children children-contexts))))

(defn
  reconstruct-pov
  "Reconstruct the tree from the POV of the input tree."
  [tree contexts]
  (if (empty? contexts)
    tree
    (let [[{parent-tag :parent-tag
            lefts :lefts
            rights :rights}
           & rest-contexts] contexts]
      (conj tree
            (reconstruct-pov
             (into [parent-tag] (concat lefts rights))
             rest-contexts)))))

(defn of [node-tag tree]
  (let [hm (into (hash-map)
                 (map
                  (fn [x]
                    [(-> x :focus first) x])
                  (build-zippers tree [])))
        zipper (hm node-tag nil)]
    (and
     zipper
     (let [{focus :focus
            contexts :contexts} zipper]
       (reconstruct-pov focus contexts)))))

(defn dfs [target tree path]
  (let [[cur-tag & children] tree
        new-path (conj path cur-tag)]
    (if (= cur-tag target)
      new-path
      (reduce
       (fn [acc t] (or acc (dfs target t new-path)))
       nil
       children))))

(defn path-from-to [x y tree]
  (let [x-pov (of x tree)]
    (dfs y x-pov [])))
