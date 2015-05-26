#include "DynArr.h"
#include "Token.h"
#include "SExp.h"

#ifndef _JAVEV_PARSER_H_
#define _JAVEV_PARSER_H_

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
#endif
