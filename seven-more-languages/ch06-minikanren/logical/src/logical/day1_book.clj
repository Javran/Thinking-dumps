(ns logical.day1-book)

(use 'clojure.core.logic)
(use 'clojure.core.logic.pldb)
(use 'logical.utils)

(db-rel mano x)
(db-rel womano x)
(db-rel vitalo p s)
(db-rel turingo p y)

(def facts
  (db
   [mano :alan-turing]
   [womano :grace-hopper]
   [mano :leslie-lamport]
   [mano :alonzo-church]
   [womano :ada-lovelace]
   [womano :barbara-liskov]
   [womano :frances-allen]
   [mano :john-mccarthy]

   [vitalo :alan-turing :dead]
   [vitalo :grace-hopper :dead]
   [vitalo :leslie-lamport :alive]
   [vitalo :alonzo-church :dead]
   [vitalo :ada-lovelace :dead]
   [vitalo :barbara-liskov :alive]
   [vitalo :john-mccarthy :dead]

   [turingo :leslie-lamport :2013]
   [turingo :barbara-liskov :2008]
   [turingo :frances-allen :2006]
   [turingo :john-mccarthy :1971]
   ))
