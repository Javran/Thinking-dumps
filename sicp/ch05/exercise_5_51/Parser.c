#include "Parser.h"
#include "Tokenizer.h"

// there is no case for tokEof,
// which is intentionally left out.
// our parser always fail (return NULL) on unrecognized
// token stream, so that we can just stop when the parser
// yields nothing. (and if the remaining part is more than just an eof,
// there must be some parsing/syntax errors.

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
        retVal = parseQuote(ps);
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

SExp *parseQuote(ParseState *ps) {
    ParseState oldPs;
    // backup old state
    memcpy(&oldPs, ps, sizeof(ParseState));
    Token *p = parseStateCurrent(ps);
    if (p->tag != tokQuote)
        goto parse_quote_exit;
    parseStateNext(ps);
    SExp *sub = parseSExp(ps);
    if (sub == NULL)
        goto parse_quote_exit;
    return newPair(newSymbol("quote"),newPair(sub,newNil()));
parse_quote_exit:
    memcpy(ps, &oldPs, sizeof(ParseState));
    return NULL;
}

void freeSExpP(SExp **p) {
    freeSExp(*p);
}

void freeSExps(DynArr *pSExpList) {
    if (pSExpList) {
        dynArrVisit(pSExpList,(DynArrVisitor)freeSExpP);
        dynArrFree(pSExpList);
        free(pSExpList);
    }
}

DynArr *parseSExps(const char *programText, FILE *errF) {
    DynArr tokenList = {0};
    dynArrInit(&tokenList, sizeof(Token));
    tokenize(programText,&tokenList,errF);
    assert( dynArrCount(&tokenList)
            && "the tokenizer should at least return tokEof"
               "making the token list non-empty");
    // it is not guaranteed that the tokenizer
    // will consume the whole file content.
    // but when something goes wrong (which prevents the full content
    // being consumed), an EOF is returned with error message printed to errF

    // at this point
    // we have at least one element in the token list,
    // which is the invariant we need to maintain
    // when calling parser.
    ParseState parseState = {0};
    parseStateInit(&tokenList,&parseState);

    DynArr *pSExpList = calloc(1, sizeof(DynArr));
    dynArrInit(pSExpList, sizeof(SExp *));

    SExp *result = NULL;
    // keep parsing results until there is an error
    // since there is no handler for tokEof,
    // an error must happen, which guarantees that
    // this loop can terminate.
    for (result = parseSExp(&parseState);
         NULL != result;
         result = parseSExp(&parseState)) {
        SExp **newExp = dynArrNew(pSExpList);
        *newExp = result;
    }
    // it is guaranteed that parseStateCurrent always produces
    // a valid pointer. no check is necessary.
    char parseFailed = ! ( tokEof == parseStateCurrent(&parseState)->tag );
    if (parseFailed) {
        fprintf(errF, "Remaining tokens:\n");
        while (parseStateLookahead(&parseState)) {
            printToken(errF, parseStateCurrent(&parseState) );
            parseStateNext(&parseState);
        }
        fputc('\n',errF);
        freeSExps(pSExpList);
        pSExpList = NULL;
    }
    dynArrVisit(&tokenList,(DynArrVisitor)freeToken);
    dynArrFree(&tokenList);
    return pSExpList;
}
