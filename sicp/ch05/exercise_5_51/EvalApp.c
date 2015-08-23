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

// there should be at least two forms of functions
// in order to make our programming language useful:
// * compound procedures
//   a compound procedure might be a structure that stores
//   argument list, function body and an environment in which
//   it was created. This is the basic form that lambda-calculus uses.
// * primitive procedures
//   primitive procedure allows us to actually "do computation",
//   image you have 2 numbers, a compound procedure cannot "consume"
//   the input and return something useful, because it only cares about
//   binding the actual values with arguments and evaluating the function body
//   but nothing about the value itself is known to the compound procedure.
//   therefore we need to have primitive procedures in order to deal with
//   value-manipulation. For the very original "Machine", a primitive is
//   supported by implementing "primitive operations" and wrap then so that
//   we are able to tell whether a function object is a primitive one and compound one
//   and do things accordingly. And the same thing happens here: primitive functions
//   in this implementation is just a pointer to a function that consumes input values
//   and do some computations based on these values, and generate some output.

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
