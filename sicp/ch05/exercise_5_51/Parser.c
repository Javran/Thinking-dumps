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

SExp *parseSExp(ParseState *ps) {
    ParseState oldPs;
    // backup old state
    memcpy(&oldPs, ps, sizeof(ParseState));

    SExp *retVal = parseList(ps);
    if (NULL == retVal) {
        retVal = parseAtom(ps);
    }
    if (NULL == retVal) {
        memcpy(ps, &oldPs, sizeof(ParseState));
    }
    return retVal;
}

// accepts token list and an iterator
SExp *parseList(ParseState *ps) {
    ParseState oldPs;
    // backup old state
    memcpy(&oldPs, ps, sizeof(ParseState));
    // the current one must be '('
    Token *p = parseStateCurrent(ps);

    // need to initialize these two values
    // so that when we jump to the end,
    // the pointer is properly initialized
    SExp *head = NULL;
    SExp **current = NULL;

    if (p->tag != tokLParen)
        goto parse_list_exit;
    parseStateNext(ps);
    p = parseStateCurrent(ps);

    head = newNil();
    current = &head;

    while (p->tag != tokRParen) {
        SExp *now = parseSExp(ps);
        if (NULL == now)
            goto parse_list_exit;
        p = parseStateCurrent(ps);
        SExp *newElem = newPair(now,*current);
        *current = newElem;
        current = &(newElem->fields.pairContent.cdr);
    }
    parseStateNext(ps);
    return head;

parse_list_exit:
    if (head)
        freeSExp(head);
    head = NULL;
    memcpy(ps, &oldPs, sizeof(ParseState));
    return NULL;
}

// missing cases:
// tokEof
// tokQuote

SExp *parseAtom(ParseState *ps) {
    Token *p = parseStateCurrent(ps);
    switch (p->tag) {
    case tokInteger:
        parseStateNext(ps);
        return newInteger(p->fields.integerContent);
    case tokSymbol:
        parseStateNext(ps);
        return newSymbol(p->fields.symbolName);
    case tokTrue:
        parseStateNext(ps);
        return newBool(1);
    case tokFalse:
        parseStateNext(ps);
        return newBool(0);
    case tokString:
        parseStateNext(ps);
        return newString(p->fields.stringContent);
    default:
        return NULL;
    }
}
