(load "../common/utils.scm")

(define-syntax my-fluid-let
  (rsc-macro-transformer
    (let ((xfmr (lambda (bindings . body)
                (let (
                  ; extract var names and values
                  (vars (map car bindings))
                  (vals (map cadr bindings))

                  ; foreach binding, generate a tmp sym name
                  (old-vars (map (lambda (ig) (gensym)) bindings))

                  ; we also keep a tmp sym name for the result
                  (result (gensym)))

                `(let 
                   
                   ; make pair-bindings to keep old value for each variable
                   ,(map (lambda (old-x x) `(,old-x ,x))
                          old-vars vars)

                   ; bind new values with variables
                   ,@(map (lambda (x e) `(set! ,x ,e))
                          vars vals)

                   (let 
                     ; eval the body and bind the result with symbol kept in 'result'
                     ((,result (begin ,@body)))

                     ; restore old values
                     ,@(map (lambda (x old-x) `(set! ,x ,old-x)) 
                            vars old-vars)
                     ,result))))))
      (lambda (e r)
        (apply xfmr (cdr e))))))

(define x 10)
(define y 100)

(out "origin:")
(out x)
(out y)

(fluid-let ((x 1) (y (+ y 1)))
  (out "temp:")
  (out x)
  (out y))


(out "origin:")
(out x)
(out y)

(my-fluid-let ((x 1) (y (+ y 1)))
  (out "temp:")
  (out x)
  (out y))

(out "origin:")
(out x)
(out y)

