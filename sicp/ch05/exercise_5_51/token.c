#include <stdlib.h>
#include <string.h>

#include "evaluator.h"

Token *mkTokenEof() {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_eof;
    return ptr;
}

Token *mkTokenLParen() {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_lparen;
    return ptr;
}

Token *mkTokenRParen() {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_rparen;
    return ptr;
}

Token *mkTokenQuote() {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_quote;
    return ptr;
}

Token *mkTokenTrue() {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_true;
    return ptr;
}

Token *mkTokenFalse() {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_false;
    return ptr;
}

Token *mkTokenString(const char* src) {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_string;
    size_t len = strlen(src);
    char *dst = malloc(len+1);
    memset(dst, 0, len+1);
    ptr->fields.string_content = strncpy(dst, src, len);
    return ptr;
}

Token *mkTokenSymbol(const char* src) {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_symbol;
    size_t len = strlen(src);
    char *dst = malloc(len+1);
    memset(dst, 0, len+1);
    ptr->fields.symbol_name = strncpy(dst, src, len);
    return ptr;
}

Token *mkTokenInteger(long int i) {
    Token *ptr = malloc(sizeof( Token ));
    ptr->tag = tok_symbol;
    ptr->fields.integer_content = i;
    return ptr;
}

