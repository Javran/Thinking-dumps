#include "EvalSimple.h"
#include "ManagedSExp.h"

const SExp *collectVars(const SExp *bindings) {
    if (sexpNil == bindings->tag)
        return managedNil();
    assert( sexpPair == bindings->tag
            && "unexpected data to collectVars");
    const SExp *cur = sexpCar(bindings);
    // TODO: make pair const?
    return managedPair(sexpCar(cur),
                       (void *)collectVars( sexpCdr(bindings) ));
}

const SExp *collectDefs(const SExp *bindings) {
    if (sexpNil == bindings->tag)
        return managedNil();
    assert( sexpPair == bindings->tag
            && "unexpected data to collectDefs");
    const SExp *cur = sexpCar(bindings);
    // TODO: make pair const?
    return managedPair(sexpCadr(cur),
                       (void *)collectDefs( sexpCdr(bindings) ));
}

char isLetExpression(const SExp *p) {
    // * (let (...) ...) => at least 3 elements
    if (countProperListSize(p) < 3)
        return 0;
    if (! isSymbol("let", sexpCar(p) ))
        return 0;
    // * for the second element, it is a proper list.
    //   here we ignore the case where a binding can look like "(x 1 2 3)"
    //   and this will be handled exactly like "(x 1)"
    const SExp *bindings = sexpCadr(p);
    return countProperListSize(bindings) >= 0;
}

// let-expression is transformed into a function application
const SExp *evLet(const SExp *exp, Environment *env) {
    const SExp *bindings = sexpCadr(exp);
    const SExp *vars = collectVars(bindings);
    const SExp *defs = collectDefs(bindings);
    const SExp *bodies = sexpCddr(exp);
    const SExp *newLam = managedPair( managedSymbol("lambda"),
                                      managedPair( (void*)vars,
                                                   (void*)bodies));
    const SExp *newApp = managedPair( (void *)newLam,
                                      (void *)defs);
    return evalDispatch(newApp, env);
}

SExpHandler letHandler = {
    isLetExpression,
    evLet
};
