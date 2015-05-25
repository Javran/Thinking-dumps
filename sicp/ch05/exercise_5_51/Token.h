#include "Common.h"

#ifndef _JAVEV_TOKEN_H_
#define _JAVEV_TOKEN_H_

// representing tokens
typedef enum {
    tokEof,
    tokLParen,
    tokRParen,
    tokQuote,
    tokTrue,
    tokFalse,
    tokString,
    tokSymbol,
    tokInteger
} TokenTag;

typedef union {
    char *stringContent;
    char *symbolName;
    long integerContent;
} TokenFields;

typedef struct {
    TokenTag tag;
    TokenFields fields;
} Token;

// functions related to tokens
void mkTokenEof(Token *);
void mkTokenLParen(Token *);
void mkTokenRParen(Token *);
void mkTokenQuote(Token *);
void mkTokenTrue(Token *);
void mkTokenFalse(Token *);
void mkTokenString(Token *, const char*);
void mkTokenSymbol(Token *, const char*);
void mkTokenInteger(Token *, long);
void printToken(FILE *, const Token *);
void freeToken(Token *);

#endif
