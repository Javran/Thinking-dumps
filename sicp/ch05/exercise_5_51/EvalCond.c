#include "EvalCond.h"

char isIf(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("if", sexpCar(p));
}

const SExp *evIf(const SExp *exp, Environment *env) {
    SExp *expPred = sexpCadr(exp);
    SExp *expRemaining = sexpCddr(exp);
    SExp *expConseq = sexpCar(expRemaining);
    SExp *expAlter = sexpCadr(expRemaining);

    const SExp *condResult = evalDispatch(expPred,env);
    if (condResult) {
        char isFalse = sexpBool == condResult->tag && \
            !condResult->fields.truthValue;
        return evalDispatch(isFalse
                            ? expAlter
                            : expConseq, env);
    } else {
        return NULL;
    }
}

SExpHandler ifHandler = {
    isIf,
    evIf
};
