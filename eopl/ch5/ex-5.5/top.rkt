(module top (lib "eopl.ss" "eopl")

  ;; require both recursive and register versions.
  ;; test with  (interp-run-all) or (registers-run-all)
  ;; (run-all) will run both.

  (require "top-interp.rkt")
  
  (provide run run-all)

   ;;; interface for book test ;;;
  (provide test-all)
  (define (test-all) 
    (run-all))
  
  )
