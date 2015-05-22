#include "Common.h"
#include "DynArr.h"
#include "Token.h"
#include "SExp.h"

// accepts token list and an iterator
SExp *parseList(DynArr *tl, void *it) {
    return NULL;
}

SExp *parseAtom(DynArr *tl, void *it) {
    // TODO: iterator should make progress
    // TODO: parsing state
    Token *p = it;
    switch (p->tag) {
    case tokInteger:
        return newInteger(p->fields.integerContent);
    case tokSymbol:
        return newSymbol(p->fields.symbolName);
    case tokTrue:
        return newBool(1);
    case tokFalse:
        return newBool(0);
    case tokString:
        return newString(p->fields.stringContent);
    default:
        // TODO
        assert(0);
    }
    return NULL;
}
