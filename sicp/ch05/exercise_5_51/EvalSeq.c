#include "EvalSeq.h"

char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", sexpCar(p));
}

// correponding to ev-sequence
// requires unev to store the sequence of expressions
const SExp *evSequence(const SExp *unev, Environment *env) {
    SExp *firstExp = NULL;
    // TODO: the list is assume to be non-empty
    while (! isLastExp( unev ) ) {
        firstExp = sexpCar( unev );
        // TODO
        evalDispatch1(firstExp,env);
        unev = sexpCdr( unev );
    }

    // for the last expression, there is no need
    // to keep information on stack
    // and we make a tailing call to evalDispatch
    firstExp = sexpCar( unev );
    return evalDispatch1(firstExp,env);
}

char isBegin(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("begin", sexpCar(p));
}

const SExp *evBegin(const SExp *exp, Environment *env) {
    SExp *beginActions = sexpCdr(exp);
    return evSequence(beginActions,env);
}

SExpHandler beginHandler = {
    isBegin,
    NULL,
    evBegin };
