#include "Common.h"
#include "Util.h"
#include "SExp.h"

SExp nilExp = {sexpNil, {0}};

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
    SExp *p = allocWithTag(sexpInteger);
    p->fields.truthValue = val;
    return p;
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

void freeSExp(SExp *p) {
    if (!p) return;
    switch (p->tag) {
    case sexpInteger:
    case sexpBool:
        break;
    case sexpSymbol:
        free(p->fields.symbolName);
        break;
    case sexpString:
        free(p->fields.stringContent);
        break;
    case sexpPair:
        freeSExp(p->fields.pairContent.car);
        freeSExp(p->fields.pairContent.cdr);
        break;
    case sexpNil:
        assert(p == &nilExp
               /* nil should never be allocated at run time
                */);
        return;
    }
    memset(p,0x00,sizeof(SExp));
    free(p);
}

void printSExp(FILE *f, SExp *p) {
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
        fprintf(f,"(");
        printSExp(f,p->fields.pairContent.car);
        fprintf(f," . ");
        printSExp(f,p->fields.pairContent.cdr);
        fprintf(f,")");
        break;
    }
}
