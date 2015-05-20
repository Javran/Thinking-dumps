#ifndef _JAVEV_TOKEN_H_
#define _JAVEV_TOKEN_H_

// representing tokens
typedef enum {
    tok_eof,
    tok_lparen,
    tok_rparen,
    tok_quote,
    tok_true,
    tok_false,
    tok_string,
    tok_symbol,
    tok_integer
} TokenTag;

typedef union {
    char *string_content;
    char *symbol_name;
    long integer_content;
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
void freeToken(Token *);

#endif
