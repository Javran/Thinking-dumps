#include "ManagedSExp.h"

// provide managed SExp that registers itself
// to the PointerManager at runtime

// "ManagedSExp" is assumed to have enough knowledge
// about how to deal with SExp structure properly
// so it won't register "freeSExp" for statically allocated objects
// (it's just a matter of performance as "freeSExp" won't do anything
// on a statically allocated object anyway)

const SExp *managedSymbol(const char *name) {
    const SExp *p = newSymbol(name);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

const SExp *managedString(const char *content) {
    const SExp *p = newString(content);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

const SExp *managedInteger(long val) {
    const SExp *p = newInteger(val);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

const SExp *managedBool(char val) {
    return newBool(val);
}

const SExp *managedNil() {
    return newNil();
}

const SExp *managedPair(const SExp *car, const SExp *cdr) {
    const SExp *p = newPair(car,cdr);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

const SExp *managedFuncObject(void *obj) {
    const SExp *p = newFuncObject(obj);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}
