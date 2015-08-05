#include "Primitives.h"
#include "SExp.h"
#include "PointerManager.h"

long *primPlusFoldHelper(long *acc, const SExp **pelem) {
    if (! acc)
        return NULL;
    const SExp *elem = *pelem;
    assert(sexpInteger == elem->tag);
    *acc = *acc + elem->fields.integerContent;
    return acc;
}

const SExp *primPlus(const SExp *args) {
    DynArr *argsA = sexpProperListToDynArr(args);
    long seed = 0;
    long *result =
        dynArrFoldLeft(argsA,
                       (DynArrFoldLeftAccumulator)primPlusFoldHelper,
                       &seed);
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

// primitives are allocated statically
// so no resource de-allocation
// is actually happening for them.
SExp primPlusSExp = {
    sexpFuncObj,
    { .pFuncObj = &primPlusObj
    }
};

long *primMinusFoldHelper(long *acc, const SExp **pelem) {
    if (! acc)
        return NULL;
    const SExp *elem = *pelem;
    assert(sexpInteger == elem->tag);
    *acc = *acc - elem->fields.integerContent;
    return acc;
}

const SExp *primMinus(const SExp *args) {
    // the operand list cannot be empty
    if (sexpNil == args->tag)
        return NULL;
    const SExp *head = sexpCar(args);
    const SExp *tail = sexpCdr(args);
    DynArr *argsA = sexpProperListToDynArr(tail);
    long seed = head->fields.integerContent;
    long *result =
        dynArrFoldLeft(argsA,
                       (DynArrFoldLeftAccumulator)primMinusFoldHelper,
                       &seed);
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

FuncObj primMinusObj = {
    funcPrim,
    { .primHdlr = primMinus }
};

SExp primMinusSExp = {
    sexpFuncObj,
    { .pFuncObj = &primMinusObj
    }
};

long *primMultFoldHelper(long *acc, const SExp **pelem) {
    if (! acc)
        return NULL;
    const SExp *elem = *pelem;
    assert(sexpInteger == elem->tag);
    *acc = *acc * elem->fields.integerContent;
    return acc;
}

const SExp *primMult(const SExp *args) {
    DynArr *argsA = sexpProperListToDynArr(args);
    long seed = 1;
    long *result =
        dynArrFoldLeft(argsA,
                       (DynArrFoldLeftAccumulator)primMultFoldHelper,
                       &seed);
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

FuncObj primMultObj = {
    funcPrim,
    { .primHdlr = primMult }
};

SExp primMultSExp = {
    sexpFuncObj,
    { .pFuncObj = &primMultObj
    }
};

const SExp *primCons(const SExp *args) {
    DynArr *argsA = sexpProperListToDynArr(args);
    int cnt = dynArrCount(argsA);
    SExp *retVal = NULL;

    if (cnt != 2) {
        retVal = NULL;
    } else {
        SExp **it  = dynArrBegin( argsA );
        SExp *car = *it; it = dynArrNext( argsA, it);
        SExp *cdr = *it;
        // TODO: lots of things are need to be constants in future
        retVal = (void *)newPair(car,cdr);
        // TODO: too many mistakes on this...
        // we must find a solution
        pointerManagerRegister(retVal);
    }
    dynArrFree(argsA);
    free(argsA);
    return retVal;
}

FuncObj primConsObj = {
    funcPrim,
    { .primHdlr = primCons }
};

SExp primConsSExp = {
    sexpFuncObj,
    { .pFuncObj = &primConsObj
    }
};
