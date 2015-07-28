#include "Primitives.h"
#include "SExp.h"
#include "PointerManager.h"

long *primPlusFoldHelper(long *acc, const SExp *elem) {
    assert(sexpInteger == elem->tag);
    *acc = *acc + elem->fields.integerContent;
    return acc;
}

const SExp *primPlus(const SExp *args) {
     DynArr *argsA = sexpProperListToDynArr(args);
     long seed = 0;
     dynArrFoldLeft(argsA, (DynArrFoldLeftAccumulator)primPlusFoldHelper,&seed);
     dynArrFree(argsA);
     free(argsA);

     SExp *retVal = newInteger( seed );
     pointerManagerRegisterCustom(retVal, (PFreeCallback)freeSExp);
     return retVal;
}

FuncObj primPlusObj = {
    funcPrim,
    { .primHdlr = primPlus }
};
