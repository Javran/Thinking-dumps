#include "EvalApp.h"

char isApplication(const SExp *p) {

}

const SExp *evApplication(const SExp *exp, Environment *env) {

}

SExpHandler applicationHandler = {
    isApplication,
    evApplication
};
