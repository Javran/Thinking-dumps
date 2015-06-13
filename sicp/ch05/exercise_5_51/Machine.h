#include "Register.h"

#ifndef _JAVEV_MACHINE_H_
#define _JAVEV_MACHINE_H_

typedef struct {
    // INVARIANT: exp is always holding an expression
    Register exp,env,val;
    Register cont,proc,argl,unev;
} Machine;

// S-expresion related.
typedef char (*SExpPredicate)(const SExp *);
typedef void (*SExpEval)(Machine *);

// an s-expression handler
// consists of a predicate and an evaluator
typedef struct {
    // invariant: the pointer can never be NULL
    SExpPredicate pred;
    // invariant: the s-exp must meet the requirement
    // that "pred" has specified
    SExpEval eval;
} SExpHandler;

#endif
