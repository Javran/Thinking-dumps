#include "EvalCond.h"

char isIf(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("if", sexpCar(p));
}

const SExp *evIf(const SExp *exp, Environment *env) {
    const SExp *expPred = sexpCadr(exp);
    const SExp *expRemaining = sexpCddr(exp);
    const SExp *expConseq = sexpCar(expRemaining);
    const SExp *expAlter = sexpCadr(expRemaining);

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
