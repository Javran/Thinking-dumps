#include "Common.h"
#include "Util.h"
#include "SExp.h"

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

// TODO: one optimization would be sharing nil pointers.
// but the free procedure need to take that into account
SExp *newNil() {
    // no content is required
    return allocWithTag(sexpNil);
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
    case sexpNil:
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
    }
    memset(p,0x00,sizeof(SExp));
    free(p);
}
