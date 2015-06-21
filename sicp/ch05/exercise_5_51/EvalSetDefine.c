#include "Machine.h"
#include "EvalSimple.h"

char isAssignment(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("set!", sexpCar(p));
}

void evAssignment(const SExp *exp, Machine *m) {
    char *varName = sexpCadr( exp )->fields.symbolName;
    SExp *expVal = sexpCddr( exp );
    Environment *env = m->env.data.asEnv;
    evalDispatch(expVal, m);
    // void * val = m -> val;
    // envInsert(env, varName, ???)
}


/*


(define assignment-variable cadr)
(define assignment-value caddr)

(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
    ; form: (define <var> <value>)
    (cadr exp)
    ; `(cadr exp)` is a list
    (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
    ;; first form
    (caddr exp)
    ;; second form, not supported here
    ;; however, exercise 5.23 provides a desugarizer
    ;; that transforms this second form into the first form
    ;; see "normalize-define" in "./exercise_5_23_trans.scm"
    (error "not supported")))
*/
