#include "Primitives.h"
#include "SExp.h"
#include "ManagedSExp.h"
#include "PointerManager.h"

// * we are limited to construct static objects ...
// closure supports are not that good in this language.
// maybe it's better to deal with only binary functions.

// * it's a tedious task to write tests for primitive functions.
// we instead write tests in implemented programming language.
// This allows the language to do self-testing, and
// all we need to do is to trust few primtives
// (e.g. "=", "eq?"), and run a "self-testing" program to do rest
// of the task for us.

// some operations provided by the initial environment can be implemented
// using existing primitives and it is not necessay to implement them
// in this implementing programming language.
// we choose to leave only primitive functions in our initial environment.
// an optional source file can be prefixed to the real program
// to build up an environment that sets up rest of the things.

typedef struct {
    const SExp *arg1;
    const SExp *arg2;
} BinArgs;

// for extracting binary arguments from an argyment list
// returns 0 if failed (e.g. argument number mismatch)
char extractBinArgs(BinArgs *ba, const SExp *args) {
    assert(ba && "caller must provide a structure for filling in data");
    if (sexpPair == args->tag) {
        // we have at least one element
        ba->arg1 = sexpCar(args);
        const SExp *rest = sexpCdr(args);
        if (sexpPair == rest->tag) {
            ba->arg2 = sexpCar(rest);
            const SExp *rest2 = sexpCdr(rest);
            // only true when "args" has exactly two args
            return sexpNil == rest2->tag;
        }
        return 0;
    } else {
        return 0;
    }
}

long *primPlusFoldHelper(long *acc, const SExp **pelem) {
    if (! acc)
        return NULL;
    const SExp *elem = *pelem;
    assert(sexpInteger == elem->tag && "plus: invalid tag");
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
        return managedInteger(seed);
    }
}

long *primMinusFoldHelper(long *acc, const SExp **pelem) {
    if (! acc)
        return NULL;
    const SExp *elem = *pelem;
    assert(sexpInteger == elem->tag && "minus: invalid tag");
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
        return managedInteger( seed );
    }
}

long *primMultFoldHelper(long *acc, const SExp **pelem) {
    if (! acc)
        return NULL;
    const SExp *elem = *pelem;
    assert(sexpInteger == elem->tag && "mult: invalid tag");
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
        return managedInteger( seed );
    }
}

const SExp *primCons(const SExp *args) {
    BinArgs ba = {0};
    if (extractBinArgs(&ba,args)) {
        return managedPair((void *)ba.arg1,(void *)ba.arg2);
    } else {
        return NULL;
    }
}

const SExp *primCar(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        // we have exactly one arg
        return sexpCar(sexpCar(args));
    }

    return NULL;
}

const SExp *primCdr(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        return sexpCdr(sexpCar(args));
    }

    return NULL;
}

const SExp *primList(const SExp *args) {
    return args;
}

const SExp *primSymbolQ(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        return managedBool(sexpCar(args)->tag == sexpSymbol);
    }
    return NULL;
}

const SExp *primStringQ(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        return managedBool(sexpCar(args)->tag == sexpString);
    }
    return NULL;
}

const SExp *primIntegerQ(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        return managedBool(sexpCar(args)->tag == sexpInteger);
    }
    return NULL;
}

const SExp *primBooleanQ(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        return managedBool(sexpCar(args)->tag == sexpBool);
    }
    return NULL;
}

const SExp *primNullQ(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        return managedBool(sexpCar(args)->tag == sexpNil);
    }
    return NULL;
}

const SExp *primPairQ(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        return managedBool(sexpCar(args)->tag == sexpPair);
    }
    return NULL;
}

// EQ for "=" sign
const SExp *primEQ(const SExp *args) {
    BinArgs ba = {0};
    if (extractBinArgs(&ba,args)) {
        if (sexpInteger == ba.arg1->tag
            && sexpInteger == ba.arg2->tag) {
            return managedBool( ba.arg1->fields.integerContent
                                == ba.arg2->fields.integerContent );
        } else {
            return NULL;
        }
    } else {
        return NULL;
    }
}

// reading R5RS and only implement things we can support for now
// "eq?" and "eqv?" don't seem to make any difference
// "isSExpEqual" can handle most of the cases correctly,
// except that when it comes to procedures and pairs,
// a pointer comparison is used instead of going into
// the structure or throwing an error
const SExp *primEqQ(const SExp *args) {
    BinArgs ba = {0};
    if (extractBinArgs(&ba,args)) {
        if (sexpPair == ba.arg1->tag
            && sexpPair == ba.arg2->tag) {
            // location equivalence is used instead of going
            // into the structure
            return managedBool( &ba.arg1->fields.pairContent
                                == &ba.arg2->fields.pairContent );
        }

        if (sexpFuncObj == ba.arg1->tag
            && sexpFuncObj == ba.arg2->tag) {
            return managedBool( ba.arg1->fields.pFuncObj
                                == ba.arg2->fields.pFuncObj );
        }

        if (sexpFuncObj == ba.arg1->tag
            || sexpFuncObj == ba.arg2->tag) {
            // "isSExpEqual" is designed not to handle function object
            // comparison, so we have to take care of this case
            // specially as well.
            return managedBool( 0 );
        }

        return managedBool( isSExpEqual(ba.arg1, ba.arg2));
    } else {
        return NULL;
    }
}

// similar to "eq?", "isSExpEqual" is capable of handling
// most of the case except that function's equivalence cannot
// be compared so we do it manually instead
const SExp *primEqualQ(const SExp *args) {
    BinArgs ba = {0};
    if (extractBinArgs(&ba,args)) {
        if (ba.arg1->tag != ba.arg2->tag)
            return managedBool( 0 );
        // value tags should be the same
        if (sexpFuncObj == ba.arg1->tag) {
            // both are function objects
            return managedBool( ba.arg1->fields.pFuncObj
                                == ba.arg2->fields.pFuncObj );
        }
        return managedBool( isSExpEqual(ba.arg1, ba.arg2));
    } else {
        return NULL;
    }
}

const SExp *primNot(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        SExp *a = sexpCar(args);
        // (not #f) => #t
        if (sexpBool == a->tag && !a->fields.truthValue) {
            return managedBool(1);
        } else {
            return managedBool(0);
        }
    }
    return NULL;
}

// our "display" only accepts form "(display <expr>)",
// in the specification there might be one extra argument that
// specifies the ouput-port, but as a lightweight implementation
// here we are not going to implement it.
// note that "display" is supposed to print human-readable messages
// and thus does not print beginning and ending quotation marks.
const SExp *primDisplay(const SExp *args) {
    if (sexpPair == args->tag && sexpNil == sexpCdr(args)->tag) {
        // we have exactly one arg
        SExp *arg = sexpCar(args);
        displaySExp(stdout,arg);
        return managedNil();
    }
    return NULL;
}

void primErrorHelper(const SExp **pelem) {
    const SExp *elem = *pelem;
    printSExp(stdout,elem);
    putc(' ',stdout);
}

// R5RS doesn't seem to say anything about "(error _)" procedures
// so we just make our own: it prints "error signalled: " and
// all its arguments. Always results in error.
const SExp *primError(const SExp *args) {
    DynArr *argsA = sexpProperListToDynArr(args);
    dynArrVisit(argsA, (DynArrVisitor)primErrorHelper);
    dynArrFree(argsA);
    free(argsA);
    return NULL;
}

const SExp *primNewline(const SExp *args) {
    if (sexpNil == args->tag) {
        putc('\n',stdout);
        return managedNil();
    }
    return NULL;
}

FuncObj primPlusObj = {funcPrim, { .primHdlr = primPlus }};
FuncObj primMinusObj = {funcPrim, { .primHdlr = primMinus }};
FuncObj primMultObj = {funcPrim, { .primHdlr = primMult }};
FuncObj primConsObj = {funcPrim, { .primHdlr = primCons }};
FuncObj primCarObj = {funcPrim, { .primHdlr = primCar }};
FuncObj primCdrObj = {funcPrim, { .primHdlr = primCdr }};
FuncObj primListObj = {funcPrim, { .primHdlr = primList }};
FuncObj primSymbolQObj = {funcPrim, { .primHdlr = primSymbolQ }};
FuncObj primStringQObj = {funcPrim, { .primHdlr = primStringQ }};
FuncObj primIntegerQObj = {funcPrim, { .primHdlr = primIntegerQ }};
FuncObj primBooleanQObj = {funcPrim, { .primHdlr = primBooleanQ }};
FuncObj primNullQObj = {funcPrim, { .primHdlr = primNullQ }};
FuncObj primPairQObj = {funcPrim, { .primHdlr = primPairQ }};
FuncObj primEQObj = {funcPrim, { .primHdlr = primEQ }};
FuncObj primEqQObj = {funcPrim, { .primHdlr = primEqQ }};
FuncObj primEqualQObj = {funcPrim, { .primHdlr = primEqualQ }};
FuncObj primNotObj = {funcPrim, { .primHdlr = primNot }};
FuncObj primDisplayObj = {funcPrim, { .primHdlr = primDisplay }};
FuncObj primErrorObj = {funcPrim, { .primHdlr = primError }};
FuncObj primNewlineObj = {funcPrim, { .primHdlr = primNewline }};

// primitives are allocated statically
// so no resource de-allocation
// is actually happening for them.
SExp primPlusSExp = {sexpFuncObj, { .pFuncObj = &primPlusObj}};
SExp primMinusSExp = {sexpFuncObj, { .pFuncObj = &primMinusObj}};
SExp primMultSExp = {sexpFuncObj, { .pFuncObj = &primMultObj}};
SExp primConsSExp = {sexpFuncObj, { .pFuncObj = &primConsObj}};
SExp primCarSExp = {sexpFuncObj, { .pFuncObj = &primCarObj}};
SExp primCdrSExp = {sexpFuncObj, { .pFuncObj = &primCdrObj}};
SExp primListSExp = {sexpFuncObj, { .pFuncObj = &primListObj}};
SExp primSymbolQSExp = {sexpFuncObj, { .pFuncObj = &primSymbolQObj}};
SExp primStringQSExp = {sexpFuncObj, { .pFuncObj = &primStringQObj}};
SExp primIntegerQSExp = {sexpFuncObj, { .pFuncObj = &primIntegerQObj}};
SExp primBooleanQSExp = {sexpFuncObj, { .pFuncObj = &primBooleanQObj}};
SExp primNullQSExp = {sexpFuncObj, { .pFuncObj = &primNullQObj}};
SExp primPairQSExp = {sexpFuncObj, { .pFuncObj = &primPairQObj}};
SExp primEQSExp = {sexpFuncObj, { .pFuncObj = &primEQObj}};
SExp primEqQSExp = {sexpFuncObj, { .pFuncObj = &primEqQObj}};
SExp primEqualQSExp = {sexpFuncObj, { .pFuncObj = &primEqualQObj}};
SExp primNotSExp = {sexpFuncObj, { .pFuncObj = &primNotObj}};
SExp primDisplaySExp = {sexpFuncObj, { .pFuncObj = &primDisplayObj}};
SExp primErrorSExp = {sexpFuncObj, { .pFuncObj = &primErrorObj}};
SExp primNewlineSExp = {sexpFuncObj, { .pFuncObj = &primNewlineObj}};
