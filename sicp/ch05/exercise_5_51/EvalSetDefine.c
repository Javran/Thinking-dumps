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

// evaluate normalized definitions
void evNormalizedDefinition(const SExp *exp, Machine *m) {
    // form: (define <var> <value>)
    char *varName = sexpCadr( exp )->fields.symbolName;
    SExp *expVal = sexpCddr( exp );
    Environment *env = m->env.data.asEnv;
    evalDispatch(expVal, m);
    // TODO?
}
