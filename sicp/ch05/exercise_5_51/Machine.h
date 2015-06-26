#include "Register.h"

#ifndef _JAVEV_MACHINE_H_
#define _JAVEV_MACHINE_H_

// TODO: get rid of Machine,
// the machine is builtin with c language!

typedef struct {
    // TODO: potential memory leak might be caused
    // when the old value of a register
    // is not deallocated properly?
    // TODO: it might be possible to totally get rid of "env" and "val"
    // registers since they don't, and shouldn't be something available globally
    Register env,val;
    Register cont,proc,argl;
    // TODO: by being aware of the fact that we
    // are using C, we might actually get rid of "continue"
    // register by letting the compiler to take care
    // some of the control for us!
} Machine;

// S-expresion related.
typedef char (*SExpPredicate)(const SExp *);
typedef void (*SExpEval)(const SExp *, Machine *);

// an s-expression handler
// consists of a predicate and an evaluator
typedef struct {
    // invariant: the pointer can never be NULL
    SExpPredicate pred;
    // invariant: the s-exp must meet the requirement
    // that "pred" has specified
    SExpEval eval;
} SExpHandler;

void evalDispatch(const SExp *, Machine *);

#endif
