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

;; a sentence is a noun-phrase followed
;; by a verb
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))

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
