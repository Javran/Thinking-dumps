(ns clojure-codes.day-3.do.barber
  (:require 
    [clojure-codes.utils :as utils]))

; TODO: description here

(defn -main [& args]
  (def open-flag (ref false))

  (defn start-service [] (dosync (ref-set open-flag true)))
  (defn stop-service [] (dosync (ref-set open-flag false)))

  (def wait-chairs-avaliable (ref 3))

  (def barber
    (agent [0,0]))

  (defn request-hair-cut
    [[cut_count, sleep_count]]
    (loop [current_cut_count cut_count current_sleep_count sleep_count]
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
            (recur (inc current_cut_count) sleep_count)
          )
          ; if false
          (do
            [current_cut_count (inc sleep_count)] )
        )
        ; if false
        [current_cut_count sleep_count])))


  (start-service)
  (def service
    (future 
      ; keep service alive for a while
      (Thread/sleep 10000)
      (stop-service)))

  (def filler
    ; put consumers to waiting chairs if possible
    (future
      (loop [consumer_lost 0]
        (if @open-flag
          ; if true
          (do
            (Thread/sleep (+ 10 (rand-int 21)))
            (if
              (dosync
                (ensure wait-chairs-avaliable)
                (if (> @wait-chairs-avaliable 0)
                  (do
                    (alter wait-chairs-avaliable dec)
                    true)
                  false))
              ; if true
              (do
                (send barber request-hair-cut)
                (recur consumer_lost))
              ; if false
              (recur (inc consumer_lost))))
          ; if false
          consumer_lost))))

  @service
  (await barber)

  (def result @barber)
  (def consumer_lost @filler)

  (println "Consumer served count: " (first result))
  (println "Barber inactive count: " (second result))
  ; the result will be no more than 500
  ;     because the barber will always spend 20ms to cut hairs
  ; even assume full-speed of consumer's arrival
  ;     i.e. consumers' arrival interval is 10ms
  ; this program produces `497` as output with barber keeping busy (no inactive time)

  ; 'consumer lost' stands for those consumers 
  ;     who have come to the barber shop but found no free chairs and had to leave
  (println "Consumer lost   count: " consumer_lost)
)
