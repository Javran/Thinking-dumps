#include "EvalApp.h"
#include "EvalSeq.h"
#include "PointerManager.h"

// an application is a non-empty proper list
char isApplication(const SExp *p) {
    // application should be the last handler
    // at this point we can tell for sure that "p" is not nil
    // so the only thing to do here is to test
    // whether p is a proper list
    while (sexpPair == p->tag)
        p = sexpCdr(p);
    return sexpNil == p->tag;
}

void releaseTempEnv(Environment *pEnv) {
    envFree(pEnv);
    free(pEnv);
}

const SExp *evApplication(const SExp *exp, Environment *env) {
    SExp *rator = sexpCar(exp);
    SExp *rands = sexpCdr(exp);

    // evaluate operator and make sure it is a lambda object
    const SExp *ratorLam = evalDispatch(rator, env);
    if (!ratorLam || sexpLamObj != ratorLam->tag) {
        return NULL;
    }
    LambdaObject *lo = ratorLam->fields.pLamObj;
    Environment *pEnvArgs = calloc(1,sizeof(Environment));
    envInit(pEnvArgs);
    pointerManagerRegisterCustom(pEnvArgs, (PFreeCallback)releaseTempEnv);
    envSetParent(pEnvArgs, lo->env);
    SExp *argsLam = lo->parameters;

    while (sexpNil != rands->tag && sexpNil != argsLam->tag ) {
        if (sexpSymbol != sexpCar( argsLam ))
            return NULL;
        char *varName = sexpCar( argsLam )->fields.symbolName;
        SExp *rand = sexpCar( rands );
        const SExp *result = evalDispatch(rand, env);
        // TODO: change this after the type of envInsert is corrected
        envInsert(pEnvArgs, varName, (void *)result);
    }

    if (! (sexpNil == rands->tag && sexpNil == argsLam->tag) )
        return NULL;

    // execute body under new environment
    return evSequence(lo->body, pEnvArgs);
}

SExpHandler applicationHandler = {
    isApplication,
    evApplication
};
