(load "../common/utils.scm")
(load "../common/test-utils.scm")

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

;; a noun-phrase is an article followed by a noun
(define (parse-noun-phrase)
  (list 'noun-phrase
        (parse-word articles)
        (parse-word nouns)))

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

(end-script)

;; Local variables:
;; proc-entry: ""
;; End:
