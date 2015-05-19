#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdio.h>

#include "evaluator.h"

void mkTokenEof(Token *tp)      { tp->tag = tok_eof;    }
void mkTokenLParen(Token *tp)   { tp->tag = tok_lparen; }
void mkTokenRParen(Token *tp)   { tp->tag = tok_rparen; }
void mkTokenQuote(Token *tp)    { tp->tag = tok_quote;  }
void mkTokenTrue(Token *tp)     { tp->tag = tok_true;   }
void mkTokenFalse(Token *tp)    { tp->tag = tok_false;  }

void mkTokenString(Token *tp, const char* src) {
    tp->tag = tok_string;
    size_t len = strlen(src);
    char *dst = malloc(len+1);
    memset(dst, 0, len+1);
    tp->fields.string_content = strncpy(dst, src, len);
}

void mkTokenSymbol(Token *tp, const char* src) {
    tp->tag = tok_symbol;
    size_t len = strlen(src);
    char *dst = malloc(len+1);
    memset(dst, 0, len+1);
    tp->fields.symbol_name = strncpy(dst, src, len);
}

void mkTokenInteger(Token *tp, long i) {
    tp->tag = tok_integer;
    tp->fields.integer_content = i;
}

void freeToken(Token *tp) {
    if (tp) {
        switch (tp->tag) {
        case tok_eof:
        case tok_lparen:
        case tok_rparen:
        case tok_quote:
        case tok_true:
        case tok_false:
        case tok_integer:
            break;
        case tok_string:
            free(tp->fields.string_content);
            tp->fields.string_content = NULL;
            break;
        case tok_symbol:
            free(tp->fields.symbol_name);
            tp->fields.symbol_name = NULL;
            break;
        default:
            assert( 0 );
        }
    }
}
