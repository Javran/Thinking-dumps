#include "EvalApp.h"
#include "EvalSeq.h"
#include "FunctionObject.h"
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

// TODO: we actually need another form of value: primitives
// * document
const SExp *evApplication(const SExp *exp, Environment *env) {
    const SExp *rator = sexpCar(exp);
    const SExp *rands = sexpCdr(exp);

    // evaluate operator and make sure it is a lambda object
    const SExp *ratorLam = evalDispatch(rator, env);
    if (!ratorLam || sexpFuncObj != ratorLam->tag) {
        return NULL;
    }
    FuncObj *fo = ratorLam->fields.pFuncObj;
    return funcObjApp(fo, rands, env);
}

SExpHandler applicationHandler = {
    isApplication,
    evApplication
};
