(ns clojure-codes.day-3.do.barber
  (:require 
    [clojure-codes.utils :as utils]))

(defn -main [& args]
  (def open-flag (ref false))

  (defn start-service [] (dosync (ref-set open-flag true)))
  (defn stop-service [] (dosync (ref-set open-flag false)))

  (def wait-chairs-avaliable (ref 3))

  (def barber
    (agent 0))

  (defn request-hair-cut
    [cut_count]
    (loop [current_cut_count cut_count]
      (if @open-flag
        ; if true
        (if (dosync
              (ensure wait-chairs-avaliable)
              (if (< @wait-chairs-avaliable 3)
                (do
                  (alter wait-chairs-avaliable inc)
                  true)
                false))
          ; if true
          (do
            (Thread/sleep 20)
            (recur (inc current_cut_count))
          )
          ; if false
          (do
            current_cut_count)
        )
        ; if false
        current_cut_count)))


  (start-service)
  (def service
    (future 
      ; keep service alive for a while
      (Thread/sleep 10000)
      (stop-service)))

  (def filler
    ; put consumers to waiting chairs if possible
    (future
      (loop []
        (if @open-flag
          (do
            (Thread/sleep (+ 10 (rand-int 21)))
            (dosync
              (ensure wait-chairs-avaliable)
              (if (> @wait-chairs-avaliable 0)
                (alter wait-chairs-avaliable dec)))
            (send barber request-hair-cut)
            (recur))))))

  @service
  (await barber)

  (println @barber)
  ; the result will be no more than 500
  ;     because the barber will always spend 20ms to cut hairs
  ; even assume full-speed of consumer's arrival
  ;     i.e. consumers' arrival interval is 10ms
  ; this program produces `497` as output
)
