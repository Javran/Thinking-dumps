#include "EvalSeq.h"

// correponding to ev-sequence
// requires unev to store the sequence of expressions
const SExp *evSequence(const SExp *unev, Environment *env) {
    const SExp *firstExp = NULL;

    if (! unev || sexpPair != unev->tag) {
        // either something wrong is happening (unev == NULL)
        // or the tag of unev is unexpected (expecting a non-empty list
        // whose head should be a pair
        return NULL;
    }
    // now we can safely assume the list to be non-empty
    while (! isLastExp( unev ) ) {
        firstExp = sexpCar( unev );
        evalDispatch(firstExp,env);
        unev = sexpCdr( unev );
        // it is more safe to check the value of unev
        // to make sure it is a non-empty list.
        // but let's assume the "isBegin" predicate has already
        // taken care of that (yes we are being sloppy here)
    }
    // for the last expression, there is no need
    // to keep information on stack
    // and we make a tailing call to evalDispatch
    firstExp = sexpCar( unev );
    return evalDispatch(firstExp,env);
}

char isBegin(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("begin", sexpCar(p));
}

const SExp *evBegin(const SExp *exp, Environment *env) {
    return evSequence(sexpCdr(exp),env);
}

SExpHandler beginHandler = {
    isBegin,
    evBegin
};
