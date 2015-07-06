#ifndef _JAVEV_MACHINE_H_
#define _JAVEV_MACHINE_H_

#include "SExp.h"
#include "Environment.h"

// TODO: get rid of Machine,
// the machine is builtin with c language!
// TODO: document about this

// S-expresion related.
typedef char (*SExpPredicate)(const SExp *);
typedef const SExp *(*SExpEval)(const SExp *, Environment *);

// TODO: overhaul plan:
// * make eval-handler return "SExp *" and accept also an environment
// * eliminate Machine
// * explain why we don't need a Machine
// * document

// an s-expression handler
// consists of a predicate and an evaluator
typedef struct {
    // invariant: the pointer can never be NULL
    SExpPredicate pred;
    // invariant: the s-exp must meet the requirement
    // that "pred" has specified
    SExpEval eval;
} SExpHandler;

const SExp *evalDispatch(const SExp *, Environment *);

#endif
