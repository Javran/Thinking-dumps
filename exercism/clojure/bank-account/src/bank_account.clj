(ns bank-account)

(defn open-account [] (atom 0))

(defn close-account [a] (reset! a nil))

(def get-balance deref)

(defn update-balance [a incr] (swap! a (partial + incr)))
