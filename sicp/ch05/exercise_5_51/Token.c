#include "Common.h"
#include "Token.h"
#include "Util.h"

void mkTokenEof(Token *tp)      { tp->tag = tokEof;    }
void mkTokenLParen(Token *tp)   { tp->tag = tokLParen; }
void mkTokenRParen(Token *tp)   { tp->tag = tokRParen; }
void mkTokenQuote(Token *tp)    { tp->tag = tokQuote;  }
void mkTokenTrue(Token *tp)     { tp->tag = tokTrue;   }
void mkTokenFalse(Token *tp)    { tp->tag = tokFalse;  }

void mkTokenString(Token *tp, const char* src) {
    tp->tag = tokString;
    tp->fields.stringContent = allocCopyString(src);
}

void mkTokenSymbol(Token *tp, const char* src) {
    tp->tag = tokSymbol;
    tp->fields.symbolName = allocCopyString(src);
}

void mkTokenInteger(Token *tp, long i) {
    tp->tag = tokInteger;
    tp->fields.integerContent = i;
}

void freeToken(Token *tp) {
    if (tp) {
        switch (tp->tag) {
        case tokEof:
        case tokLParen:
        case tokRParen:
        case tokQuote:
        case tokTrue:
        case tokFalse:
        case tokInteger:
            break;
        case tokString:
            free(tp->fields.stringContent);
            tp->fields.stringContent = NULL;
            break;
        case tokSymbol:
            free(tp->fields.symbolName);
            tp->fields.symbolName = NULL;
            break;
        default:
            assert( 0 );
        }
    }
}
