#include "EvalApp.h"

// an application is a non-empty proper list
char isApplication(const SExp *p) {
    // application should be the last handler
    // at this point we can tell for sure that "p" is not nil
    // so the only thing to do here is to test
    // whether p is a proper list
    return sexpNil == p->tag
        || (sexpPair == p->tag &&
            isApplication(sexpCdr(p)));
}

const SExp *evApplication(const SExp *exp, Environment *env) {

}

SExpHandler applicationHandler = {
    isApplication,
    evApplication
};
