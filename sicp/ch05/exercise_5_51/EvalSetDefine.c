#include "EvalSetDefine.h"
#include "PointerManager.h"

char isAssignment(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("set!", sexpCar(p));
}

const SExp *evAssignment(const SExp *exp, Environment *env) {
    char *varName = sexpCadr( exp )->fields.symbolName;
    SExp *expVal = sexpCar(sexpCddr( exp ));
    FrameEntry *fe = envLookup(env, varName);
    if (fe) {
        const SExp *result = evalDispatch(expVal, env);
        if (result) {
            fe->val = (void *) result;
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
        // TODO: these run-time-allocated objects are causing troubles
        // (cons 'lambda (cons args body))
        expVal = newPair(newSymbol( "lambda" ),
                         newPair(args, body));

    }

    char *varName = expVar->fields.symbolName;
    const SExp *result = evalDispatch(expVal, env);
    envInsert(env, varName, (void *) result);
    // TODO: should we assume nil to be a static object?
    return newNil();
}

SExpHandler definitionHandler = {
    isDefinition,
    evDefinition
};
