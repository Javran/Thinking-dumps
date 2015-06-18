#include "Register.h"

#ifndef _JAVEV_MACHINE_H_
#define _JAVEV_MACHINE_H_

typedef struct {
    // INVARIANT: exp is always holding an expression
    Register exp,env,val;
    Register cont,proc,argl,unev;
    // TODO: by being aware of the fact that we
    // are using C, we might actually get rid of "continue"
    // register by letting the compiler to take care
    // some of the control for us!
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

void evalDispatch(Machine *);

#endif
