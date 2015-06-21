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
    evalDispatch(expVal, m);
    // TODO: what should be stored in the environment?
    // void * val = m -> val;
    // envInsert(env, varName, ???)
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
