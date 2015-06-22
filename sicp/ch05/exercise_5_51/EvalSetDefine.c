#include "Machine.h"
#include "EvalSimple.h"

char isAssignment(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("set!", sexpCar(p));
}

void evAssignment(const SExp *exp, Machine *m) {
    char *varName = sexpCadr( exp )->fields.symbolName;
    SExp *expVal = sexpCddr( exp );
    Environment *env = m->env.data.asEnv;
    FrameEntry *fe = envLookup(env, varName);
    assert( fe /* the frame entry must exist */ );
    evalDispatch(expVal, m);
    const SExp *result = m->val.data.asSExp;
    fe->val = (void *) result;
    // TODO: not sure if this decision is correct,
    // but let's store SExp * in the environment
}

char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", sexpCar(p));
}

void evDefinition(const SExp *exp, Machine *m) {
    SExp *expLook = sexpCadr( exp );
    SExp *expVar = NULL;
    SExp *expVal = NULL;
    if ( sexpSymbol == expLook->tag ) {
        // form: (define <var> <val>)
        expVar = expLook;
        expVal = sexpCar(sexpCddr( exp ));
    } else {
        // form: (define (<var> <args> ...) <exps> ...)
        expVar = sexpCar( expLook );
        SExp *args = sexpCdr(expLook);
        SExp *body = sexpCddr( exp );
        // (cons 'lambda (cons args body))
    }

    char *varName = expVar->fields.symbolName;
//    SExp *expVal = sexpCddr( exp );
    Environment *env = m->env.data.asEnv;
    evalDispatch(expVal, m);
    // TODO?
}


