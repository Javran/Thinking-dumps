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
        fprintf(stderr,"list failed\n");
        retVal = parseAtom(ps);
    }
    if (NULL == retVal) {
        fprintf(stderr,"atom failed\n");
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
    if (p->tag != tokLParen)
        goto parse_list_exit;
    parseStateNext(ps);
    p = parseStateCurrent(ps);
    if (p->tag == tokRParen) {
        // found "()"
        parseStateNext(ps);
        return newNil();
    } else {
        // TODO: impl is wrong,
        // try loop.
        SExp* car = parseAtom(ps);
        if (car == NULL) {
            puts("car failed");
            goto parse_list_exit;
        }
        SExp* cdr = parseList(ps);
        if (cdr == NULL) {
            puts("cdr failed");
            goto parse_list_exit;
        }
        return newPair(car,cdr);
    }

parse_list_exit:
    memcpy(ps, &oldPs, sizeof(ParseState));
    return NULL;
}

// missing cases:
// tokEof
// tokLParen
// tokRParen
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
