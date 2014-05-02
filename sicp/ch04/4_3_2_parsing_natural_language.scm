(load "../common/utils.scm")
(load "../common/test-utils.scm")

(load "./amb-eval.scm")

(define (run-source-in-env src)
  `(begin
     ;; definition of all valid objects

     (define nouns
       '(noun
         student professor cat class))

     (define verbs
       '(verb
         studies lectures eats sleeps))

     (define articles
       '(article
         the a))

     (define prepositions
       '(prep
         for to in by with))

     ;; given a word list, try to parse the next data
     (define (parse-word word-list)
       (require (not (null? *unparsed*)))
       (require (memq (car *unparsed*)
                      (cdr word-list)))
       (let ((found-word (car *unparsed*)))
         (set! *unparsed* (cdr *unparsed*))
         (list (car word-list) found-word)))

     ;; prepostional phrase is a preposition followed by noun phrase
     (define (parse-prepositional-phrase)
       (list 'prep-phrase
             (parse-word prepositions)
             (parse-noun-phrase)))

     ;; now a sentence is noun phrase + verb phrase
     (define (parse-sentence)
       (list 'sentence
             (parse-noun-phrase)
             (parse-verb-phrase)))

     ;; verb phrase: a verb (maybe followed by prepositional phrase
     ;; e.g.: * eats to a cat with the cat ..
     ;;       * studies with a student
     ;;       * lectures
     ;; (well we don't do sanity check here)
     (define (parse-verb-phrase)
       (define (maybe-extend verb-phrase)
         (amb verb-phrase
              (maybe-extend
               (list 'verb-phrase
                     verb-phrase
                     (parse-prepositional-phrase)))))
       (maybe-extend (parse-word verbs)))

     ;; a simple noun phrase is an article followed by a noun
     (define (parse-simple-noun-phrase)
       (list 'simple-noun-phrase
             (parse-word articles)
             (parse-word nouns)))


     ;; a noun phrase is: a simple one, might be followed by props
     (define (parse-noun-phrase)
       (define (maybe-extend noun-phrase)
         (amb noun-phrase
              (maybe-extend
               (list 'noun-phrase
                     noun-phrase
                     (parse-prepositional-phrase)))))
       (maybe-extend (parse-simple-noun-phrase)))

     ;; the data to be converted
     (define *unparsed* '())

     ;; parse input
     (define (parse input)
       (set! *unparsed* input)
       (let ((sent (parse-sentence)))
         ;; when parsing is done,
         ;; there shouldn't be anything remaining
         (require (null? *unparsed*))
         sent))
     ,src
     ))

;; test for nondeterminism ..
;; 

;; should be only single one result:
(for-each
 out
 (amb-eval-all
  (run-source-in-env
   `(parse '(the student with the cat sleeps in the class)))
  (amb-init-env)))
(newline)

(for-each
 out
 (amb-eval-all
  (run-source-in-env
   `(parse '(the professor lectures to the student with the cat)))
  (amb-init-env)))

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
