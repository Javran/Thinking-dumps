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
typedef char (*SExpPredicate)(SExp *);
typedef void (*SExpEval)(Machine *);

// an s-expression handler
// consists of a predicate and an evaluator
typedef struct {
    SExpPredicate pred;
    SExpEval eval;
} SExpHandler;
