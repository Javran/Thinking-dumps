#include "Primitives.h"
#include "SExp.h"
#include "PointerManager.h"

long *primPlusFoldHelper(long *acc, const SExp *elem) {
    if (! acc)
        return NULL;
    assert(sexpInteger == elem->tag);
    *acc = *acc + elem->fields.integerContent;
    return acc;
}

const SExp *primPlus(const SExp *args) {
    DynArr *argsA = sexpProperListToDynArr(args);
    long seed = 0;
    long *result = dynArrFoldLeft(argsA, (DynArrFoldLeftAccumulator)primPlusFoldHelper,&seed);
    dynArrFree(argsA);
    free(argsA);

    if (!result) {
        return NULL;
    } else {
        SExp *retVal = newInteger( seed );
        pointerManagerRegisterCustom(retVal, (PFreeCallback)freeSExp);
        return retVal;
    }
}


FuncObj primPlusObj = {
    funcPrim,
    { .primHdlr = primPlus }
};

// TODO: I think it's getting tricky now ...
SExp primPlusSExp = {
    sexpFuncObj,
    { .pFuncObj = &primPlusObj
    }
};
