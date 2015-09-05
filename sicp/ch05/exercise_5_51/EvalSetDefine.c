#include "EvalSetDefine.h"
#include "PointerManager.h"
#include "ManagedSExp.h"

char isAssignment(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("set!", sexpCar(p));
}

const SExp *evAssignment(const SExp *exp, Environment *env) {
    char *varName = sexpCadr( exp )->fields.symbolName;
    const SExp *expVal = sexpCar(sexpCddr( exp ));
    FrameEntry *fe = envLookup(env, varName);
    if (fe) {
        const SExp *result = evalDispatch(expVal, env);
        if (result) {
            fe->val = result;
            return newNil();
        } else {
            return NULL;
        }
    } else {
        return NULL;
    }
}

SExpHandler assignmentHandler = {
    isAssignment,
    evAssignment
};

char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", sexpCar(p));
}

const SExp *evDefinition(const SExp *exp, Environment *env) {
    const SExp *expLook = sexpCadr( exp );
    const SExp *expVar = NULL;
    const SExp *expVal = NULL;
    if ( sexpSymbol == expLook->tag ) {
        // form: (define <var> <val>)
        expVar = expLook;
        expVal = sexpCar(sexpCddr( exp ));
    } else {
        // form: (define (<var> <args> ...) <exps> ...)
        expVar = sexpCar( expLook );
        const SExp *args = sexpCdr(expLook);
        const SExp *body = sexpCddr( exp );
        // (cons 'lambda (cons args body))
        expVal = managedPair(managedSymbol( "lambda" ),
                             managedPair(args, body));
    }

    char *varName = expVar->fields.symbolName;
    const SExp *result = evalDispatch(expVal, env);
    envInsert(env, varName, result);
    return managedNil();
}

SExpHandler definitionHandler = {
    isDefinition,
    evDefinition
};
