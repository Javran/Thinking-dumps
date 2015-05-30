#include "Common.h"
#include "SExp.h"

// TODO: for now I have no idea about what could
// be a valid value for register to hold
typedef void *RegVal;

typedef struct {
    RegVal exp,env,val;
    RegVal cont,proc,argl,unev;
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

char isSelfEvaluating(const SExp *p) {
    switch (p->tag) {
    case sexpInteger:
    case sexpString:
    case sexpBool:
        return 1;
    default:
        return 0;
    }
}

void evalSelfEvaluating(Machine *m) {
    m->val = m->exp;
}

SExpHandler selfEvaluatingHandler = {
    isSelfEvaluating,
    evalSelfEvaluating };
