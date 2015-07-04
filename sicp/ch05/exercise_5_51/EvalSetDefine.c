#include "EvalSetDefine.h"
#include "PointerManager.h"

char isAssignment(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("set!", sexpCar(p));
}

const SExp *evAssignment(const SExp *exp, Environment *env) {
    char *varName = sexpCadr( exp )->fields.symbolName;
    SExp *expVal = sexpCddr( exp );
    FrameEntry *fe = envLookup(env, varName);
    if (fe) {
        const SExp *result = evalDispatch1(expVal, env);
        fe->val = (void *) result;
        const SExp *retVal = newNil();
        pointerManagerRegister((void *)retVal);
        return retVal;
    } else {
        return NULL;
    }
}

SExpHandler assignmentHandler = {
    isAssignment,
    NULL,
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
        // (cons 'lambda (cons args body))
        expVal = newPair(newSymbol( "lambda" ),
                         newPair(args, body));
    }
    char *varName = expVar->fields.symbolName;
    const SExp *result = evalDispatch1(expVal, env);
    envInsert(env, varName, (void *) result);

    const SExp *retVal = newNil();
    pointerManagerRegister((void *)retVal);
    return retVal;

}

SExpHandler definitionHandler = {
    isDefinition,
    NULL,
    evDefinition
};
