#include "EvalSeq.h"

char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", sexpCar(p));
}

// correponding to ev-sequence
// requires unev to store the sequence of expressions
void evSequence(const SExp *unev, Machine *m) {
    SExp *firstExp = NULL;
    // TODO: the list is assume to be non-empty
    while (! isLastExp( unev ) ) {
        firstExp = sexpCar( unev);
        evalDispatch(firstExp,m);
        unev = sexpCdr( unev );
    }

    firstExp = sexpCar( unev );
    // otherwise this is the last expression, transfer control
    evalDispatch(firstExp,m);
}

char isBegin(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("begin", sexpCar(p));
}

void evBegin(const SExp *exp, Machine *m) {
    SExp *beginActions = sexpCdr(exp);
    evSequence(beginActions,m);
}

SExpHandler beginHandler = {
    isBegin,
    evBegin };

// TODO: definition should not belong to this file
void evDefinition(Machine *m) {
    SExp *exp = m->exp.data.asSExp;

    // TODO: function definition
    SExp *expVar = exp->fields.pairContent.cdr->fields.pairContent.car;
    SExp *expBody = exp->fields.pairContent.cdr->fields.pairContent.cdr;

    // TODO: need eval!
}
