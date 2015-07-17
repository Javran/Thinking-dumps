#ifndef JAVEV_PARSER_H
#define JAVEV_PARSER_H

#include "DynArr.h"
#include "Token.h"
#include "SExp.h"

typedef struct {
    DynArr *tokenList;
    Token *current;
    Token *lookahead;
} ParseState;

void parseStateInit(DynArr *, ParseState *);
Token *parseStateCurrent(const ParseState *);
Token *parseStateLookahead(const ParseState *);
void parseStateNext(ParseState *);

SExp *parseList(ParseState *);
SExp *parseAtom(ParseState *);
SExp *parseSExp(ParseState *);
SExp *parseQuote(ParseState *);

void freeSExps(DynArr *);
DynArr *parseSExps(const char *, FILE *);
#endif
