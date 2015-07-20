#include "EvalApp.h"
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

const SExp *evApplication(const SExp *exp, Environment *env) {
    SExp *rator = sexpCar(exp);
    SExp *rands = sexpCdr(exp);

    // evaluate operator and make sure it is a lambda object
    const SExp *ratorLam = evalDispatch(rator, env);
    if (!ratorLam || sexpLamObj != ratorLam->tag) {
        return NULL;
    }
    Environment envArgs = {0};
    envInit(&envArgs);
    pointerManagerRegisterCustom(&envArgs, (PFreeCallback)envFree);
    envSetParent(&envArgs, ratorLam->fields.pLamObj->env);
    SExp *argsLam = ratorLam->fields.pLamObj->parameters;

    while (sexpNil != rands->tag && sexpNil != argsLam->tag ) {
    }

    if (! (sexpNil == rands->tag && sexpNil == argsLam->tag) ) {
        // we have an argument mismatch
    }

}

SExpHandler applicationHandler = {

    isApplication,
    evApplication
};
