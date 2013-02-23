(ns clojure-codes.day-3.do.barber
  (:require 
    [clojure-codes.utils :as utils]))

; Task #2: solve sleeping barber problem
;     * 3 chairs in waiting room
;     * 1 barber and 1 chair in barber shop
;     * 20 ms to cut hair
;     * consumer arriving interval 10 - 30 ms
;     * the barber shop will be open for 10 sec

(defn -main [& args]
  (def open-flag (ref false))
  (defn start-service
    []
    (dosync (ref-set open-flag true)))
  (defn stop-service
    []
    (dosync (ref-set open-flag false)))

  (def waiting-chair-avaliable (ref 3))

  (def barber (agent 0))

  (defn request-cut-hair
    [done_count]
    (println :w @waiting-chair-avaliable)
    (println :o @open-flag)

    (loop [d_c done_count]
      (if 
        (and @open-flag (< @waiting-chair-avaliable 3))
        ; we have work to do now
        (do
          (println :working d_c)
          (dosync (alter waiting-chair-avaliable inc))  
          (Thread/sleep 2)
          (recur (inc d_c))
        )
        ; nothing to do, just keep sleeping and return done_count  
        (do
          (println :sleep)
          d_c)
      )))

  (start-service)
  (def service
    (send 
      (agent ())
      (fn
        [_]
        (Thread/sleep (* 1 1000))
        (stop-service))))

  (def filler
    (send
      (agent ())
      (fn
        [_]
        (loop []
          (if @open-flag
            (do
              (Thread/sleep (+ 10 (rand-int 21)))
              (dosync
                (if (> @waiting-chair-avaliable 0)
                  (alter waiting-chair-avaliable dec)))
              ; ask barber to do hair cutting
              ;     in case barber is sleeping
              (println @waiting-chair-avaliable)

              (send barber request-cut-hair)

              ; 10 - 30 
              ; -> 10 + random [ 0 - 20 ] (in)
              ; -> 10 + random [ 0 - 21 ] (ex)
              (if @open-flag 
                (recur)
                ())
              )
            ())))))

  (Thread/sleep 100)
  (await service)

  (println @barber)

)
