#include "EvalSimple.h"
#include "FunctionObject.h"
#include "PointerManager.h"

char isLetExpression(const SExp *p) {
    // TODO
    return 0;
}


const SExp *evLet(const SExp *exp, Environment *env) {
    // TODO
    return NULL;
}

SExpHandler letHandler = {
    isLetExpression,
    evLet
};
