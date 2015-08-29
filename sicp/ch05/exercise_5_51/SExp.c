#include "Common.h"
#include "Util.h"
#include "SExp.h"
#include "DynArr.h"
#include "FunctionObject.h"

// we can have some convention here:
// * newXXX is like allocating some space, but the user should take care of the de-allocation
// * freeXXX is not usually recursive
// * mkXXX is not allocating space, but fills in some information
//   (TODO: some procedure names are inconsistent with this rule, fix it.)

// statically allocated objects,
// which are intended for sharing
// (to reduce runtime-allocation overhead)
SExp nilExp =
    {sexpNil, {0}};
// use `!!val` to limit the index to be one of {0,1},
// in which 0 stands for false and 1 stands for true.
SExp boolExps[2] = {
    {sexpBool, {.truthValue = 0}},
    {sexpBool, {.truthValue = 1}},
};

// internal use only, allocate and assign tag
// caller is responsible for finishing the object creation
SExp *allocWithTag(SExpTag t) {
    SExp *p = calloc(1,sizeof(SExp));
    p->tag = t;
    return p;
}

SExp *newSymbol(const char *name) {
    SExp *p = allocWithTag(sexpSymbol);
    p->fields.symbolName = allocCopyString(name);
    return p;
}

SExp *newString(const char *content) {
    SExp *p = allocWithTag(sexpString);
    p->fields.stringContent = allocCopyString(content);
    return p;
}

SExp *newInteger(long val) {
    SExp *p = allocWithTag(sexpInteger);
    p->fields.integerContent = val;
    return p;
}

SExp *newBool(char val) {
    return &boolExps[!!val];
}

// optimization: nil is assigned in static space
// so that it can be shared.
SExp *newNil() {
    return &nilExp;
}

SExp *newPair(SExp *car, SExp *cdr) {
    SExp *p = allocWithTag(sexpPair);
    p->fields.pairContent.car = car;
    p->fields.pairContent.cdr = cdr;
    return p;
}

SExp *newFuncObject(void *obj) {
    SExp *p = allocWithTag(sexpFuncObj);
    p->fields.pFuncObj = obj;
    return p;
}

void freeSExpRec(SExp *p) {
    if (!p) return;
    switch (p->tag) {
    case sexpInteger:
        break;
    case sexpSymbol:
        free(p->fields.symbolName);
        break;
    case sexpString:
        free(p->fields.stringContent);
        break;
    case sexpPair:
        freeSExpRec(p->fields.pairContent.car);
        freeSExpRec(p->fields.pairContent.cdr);
        break;
    // special cases for statically allocated objects
    case sexpNil:
        assert(p == &nilExp
               && "nil should never be allocated at run time");
        return;
    case sexpBool:
        assert((p == &boolExps[0] || p == &boolExps[1])
               && "boolExp should never be allocated at run time");
        return;
    case sexpFuncObj:
        freeFuncObject(p->fields.pFuncObj);
        break;
    }
    memset(p,0x00,sizeof(SExp));
    free(p);
}

void freeSExp(SExp *p) {
    if (!p) return;
    switch (p->tag) {
    case sexpInteger:
        break;
    case sexpSymbol:
        free(p->fields.symbolName);
        break;
    case sexpString:
        free(p->fields.stringContent);
        break;
    case sexpPair:
        // non-recursive free won't go into structures
        break;
    // special cases for statically allocated objects
    case sexpNil:
        assert(p == &nilExp
               && "nil should never be allocated at run time");
        return;
    case sexpBool:
        assert((p == &boolExps[0] || p == &boolExps[1])
               && "boolExp should never be allocated at run time");
        return;
    case sexpFuncObj:
        freeFuncObject(p->fields.pFuncObj);
        break;
    }
    memset(p,0x00,sizeof(SExp));
    free(p);
}

// this function is only intended to be called by "printPairR"
void printPairR(FILE *f, const SExp *p) {
    // according to the situation, the following things might happen:
    switch (p->tag) {
    case sexpNil:
        // the "cdr" part (of the parent pair) is empty, output ")"
        // making the whole output to be "({a})"
        fputc(')', f); return;
    case sexpPair:
        // we have another pair here, in this case
        // the "car" part is outputed first to make the output like:
        // "({a} {b}", then notice this is the perfect situation for
        // printPairR to run recursively.
        fputc(' ', f);
        printSExp(f,p->fields.pairContent.car);
        printPairR(f,p->fields.pairContent.cdr);
        return;
    default:
        // finally, if none of the above matches,
        // we are facing a improper list,
        // in this case " . {b})" is outputed to make
        // the whole thing look like "({a} . {b})"
        // TODO:
        // since for now we don't have a parser for
        // parsing improper list,
        // this part of the implementation is unconfirmed.
        // TODO: now I can confirm it's working
        // let's put the testcase somewhere.
        fputs(" . ", f);
        printSExp(f,p);
        fputc(')', f);
        return;
    }
}

// for "display", almost the same as "printPairR"
void displayPairR(FILE *f, const SExp *p) {
    switch (p->tag) {
    case sexpNil:
        fputc(')', f); return;
    case sexpPair:
        fputc(' ', f);
        displaySExp(f,p->fields.pairContent.car);
        displayPairR(f,p->fields.pairContent.cdr);
        return;
    default:
        fputs(" . ", f);
        displaySExp(f,p);
        fputc(')', f);
        return;
    }
}

// pretty prints a pair by first outputing
// "({a}" and then transfering control to "printPairR"
// to generate rest of the output.
void printPairL(FILE *f, const SExp *p) {
    assert(p && p->tag == sexpPair
           && "the second argument should be a valid object of sexpPair");
    fputc('(',f);
    printSExp(f,p->fields.pairContent.car);
    printPairR(f,p->fields.pairContent.cdr);
}

// for "display", almost the same as "displayPairL"
void displayPairL(FILE *f, const SExp *p) {
    assert(p && p->tag == sexpPair
           && "the second argument should be a valid object of sexpPair");
    fputc('(',f);
    displaySExp(f,p->fields.pairContent.car);
    displayPairR(f,p->fields.pairContent.cdr);
}

// internal use only, print values that are neither strings nor pairs
void printNonStringPrimitives(FILE *f, const SExp *p) {
    switch (p->tag) {
    case sexpSymbol:
        fputs(p->fields.symbolName,f);
        return;
    case sexpInteger:
        fprintf(f,"%ld", p->fields.integerContent);
        return;
    case sexpBool:
        fprintf(f,p->fields.truthValue? "#t": "#f");
        return;
    case sexpNil:
        fprintf(f, "()");
        return;
    case sexpFuncObj:
        fprintf(f,"<FuncObj:%p>",(void *)p->fields.pFuncObj);
        return;
    default:
        assert(0 && "printNonStringPrimitives gets invalid input");
    }
}

void printSExp(FILE *f, const SExp *p) {
    switch (p->tag) {
    case sexpSymbol:
        // in case '%' gets accidentally handled...
        fprintf(f, "%s", p->fields.symbolName);
        break;
    case sexpString:
        fprintf(f,"\"%s\"", p->fields.stringContent);
        break;
    case sexpInteger:
        fprintf(f,"%ld", p->fields.integerContent);
        break;
    case sexpBool:
        fprintf(f,p->fields.truthValue? "#t": "#f");
        break;
    case sexpNil:
        fprintf(f, "()");
        break;
    case sexpPair:
        printPairL(f,p);
        break;
    case sexpFuncObj:
        fprintf(f,"<FuncObj:%p>",(void *)p->fields.pFuncObj);
        break;
    }
}

// TODO: this is awkward, the purpose of having "displaySExp"
// is to support the intended behavior for displaying strings,
// as the quotation mark should not be displayed.
// in order to do so, we have to copy everything in "printSExp"
// just to make this small modification
// I'm thinking about having a function to handle basic cases
// (primitive values, list pretty-printing) and leave some unknown thing
// as arguments.
void displaySExp(FILE *f, const SExp *p) {
    switch (p->tag) {
    case sexpSymbol:
        // in case '%' gets accidentally handled...
        fprintf(f, "%s", p->fields.symbolName);
        break;
    case sexpString:
        fprintf(f,"%s", p->fields.stringContent);
        break;
    case sexpInteger:
        fprintf(f,"%ld", p->fields.integerContent);
        break;
    case sexpBool:
        fprintf(f,p->fields.truthValue? "#t": "#f");
        break;
    case sexpNil:
        fprintf(f, "()");
        break;
    case sexpPair:
        displayPairL(f,p);
        break;
    case sexpFuncObj:
        fprintf(f,"<FuncObj:%p>",(void *)p->fields.pFuncObj);
        break;
    }
}

DynArr *sexpListToDynArr(const SExp *exp) {
    DynArr *da = calloc(1,sizeof(DynArr));
    dynArrInit(da,sizeof(SExp *));
    while (sexpNil != exp->tag) {
        SExp ** p = dynArrNew(da);
        *p = exp->fields.pairContent.car;
        exp = exp->fields.pairContent.cdr;
    }
    return da;
}

// INVARIANT: e1 and e2 are both non-NULL
// if we are comparing 2 concrete values, a NULL value
// should never appear in the AST (note that nil object
// is represented as a static object rather than
// using NULL directly, this might not be necessary,
// but it helps disambiguating a null value from implementing language
// and a null value from implemended language
char isSExpEqual(const SExp *e1, const SExp *e2) {
    assert( e1 && "e1 should not be NULL" );
    assert( e2 && "e2 should not be NULL" );
    assert( sexpFuncObj != e1->tag && sexpFuncObj != e2 -> tag
            && "cannot test equality involving LambdaObject");

    if (e1 == e2) return 1;

    // e1 and e2 are not null
    if (e1->tag == e2->tag) {
        switch(e1->tag) {
        case sexpSymbol:
            return 0 == strcmp(e1->fields.symbolName,
                               e2->fields.symbolName);
        case sexpString:
            return 0 == strcmp(e1->fields.stringContent,
                               e2->fields.stringContent);
        case sexpInteger:
            return e1->fields.integerContent
                == e2->fields.integerContent;
        case sexpBool:
            return e1->fields.truthValue
                == e2->fields.truthValue;
        case sexpNil:
            return 1;
        case sexpPair:
            return isSExpEqual(sexpCar(e1),sexpCar(e2))
                && isSExpEqual(sexpCdr(e1),sexpCdr(e2));
        case sexpFuncObj:
            assert(0 && "dead code");
        }
        assert(0 && "dead code");
    } else {
        return 0;
    }
}

DynArr *sexpProperListToDynArr(const SExp *xs) {
    DynArr *da = calloc(1, sizeof(DynArr));
    // WARNING: the user of this function is responsible
    // for freeing this object
    dynArrInit(da,sizeof(SExp *));

    const SExp *cur = xs;
    // assume the input is a proper list.
    for (cur = xs; sexpNil != cur->tag; cur = sexpCdr(cur)) {
        const SExp **p = dynArrNew(da);
        *p = sexpCar(cur);
    }
    return da;
}
