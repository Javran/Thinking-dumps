#include "ManagedSExp.h"

// provide managed SExp that registers itself
// to the PointerManager at runtime

// "ManagedSExp" is assumed to have enough knowledge
// about how to deal with SExp structure properly
// so it won't register "freeSExp" for statically allocated objects
// (it's just a matter of performance as "freeSExp" won't do anything
// on a statically allocated object anyway)

SExp *managedSymbol(const char *name) {
    SExp *p = newSymbol(name);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

SExp *managedString(const char *content) {
    SExp *p = newString(content);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

SExp *managedInteger(long val) {
    SExp *p = newInteger(val);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

SExp *managedBool(char val) {
    return newBool(val);
}

SExp *managedNil() {
    return newNil();
}

SExp *managedPair(SExp *car, SExp *cdr) {
    SExp *p = newPair(car,cdr);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}

SExp *managedFuncObject(void *obj) {
    SExp *p = newFuncObject(obj);
    pointerManagerRegisterCustom(p,(PFreeCallback)freeSExp);
    return p;
}
