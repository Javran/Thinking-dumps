#include "Parser.h"

// INVARIANT: the token list should never be empty
// and ParseState should have all fields being zero.
void parseStateInit(DynArr *tokenList, ParseState *ps) {
    assert( tokenList && dynArrCount(tokenList) );
    assert( ps
            && !ps->tokenList
            && !ps->current
            && !ps->lookahead );

    ps->tokenList = tokenList;
    ps->current = dynArrBegin(tokenList);
    ps->lookahead = dynArrNext(tokenList,ps->current);
}

Token *parseStateCurrent(const ParseState *ps) {
    return ps->current;
}

Token *parseStateLookahead(const ParseState *ps) {
    return ( ps->lookahead < (Token *)dynArrEnd(ps->tokenList) )
        ? ps->lookahead
        : NULL;
}

void parseStateNext(ParseState *ps) {
    ps->current = ps->lookahead;
    ++ ps->lookahead;
}

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
