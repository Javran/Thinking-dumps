#include "Common.h"
#include "Syntax.h"
#include "Token.h"
#include "Tokenizer.h"

void tokenize(char *curPos, DynArr *tokenList) {
    // INVARIANT: make sure every possible path
    // does a return unless EOF is reached

    char buff[SMALL_BUFFER_SIZE] = {0};
    // skip spaces so we always
    // end up with non-space for tokenizing
    while (0 != *curPos && isspace(*curPos))
        ++ curPos;

    Token *newTok = NULL;
    // static initials
    switch (*curPos) {
    case 0:
        // end of file
        newTok = dynArrNew(tokenList);
        mkTokenEof(newTok);
        return;
    case ';':
        // a comment
        while ('\n' != *curPos && 0 != *curPos)
            ++curPos;
        if (0 != *curPos) ++curPos;
        tokenize(curPos,tokenList);
        return;
    case '(':
        newTok = dynArrNew(tokenList);
        mkTokenLParen(newTok);
        tokenize(++curPos,tokenList);
        return;
    case ')':
        newTok = dynArrNew(tokenList);
        mkTokenRParen(newTok);
        tokenize(++curPos,tokenList);
        return;
    case '\'':
        newTok = dynArrNew(tokenList);
        mkTokenQuote(newTok);
        tokenize(++curPos,tokenList);
        return;
    case '#':
        ++curPos;
        // we will only handle "#t" and "#f" here ... for now
        switch (*curPos) {
        case 'T':
        case 't':
            newTok = dynArrNew(tokenList);
            mkTokenTrue(newTok);
            tokenize(++curPos,tokenList);
            return;
        case 'F':
        case 'f':
            newTok = dynArrNew(tokenList);
            mkTokenFalse(newTok);
            tokenize(++curPos,tokenList);
            return;
        default:
            fprintf(stderr,"error: not supported\n");
            return;
        }
    case '"':
        ++curPos;
        {
            char *curBuff = buff;
            // parse a string
            while (0 != *curPos && '"' != *curPos) {
                if ('\\' == *curPos) {
                    ++curPos;
                    // handle escaping
                    switch (*curPos) {
                    case 'n':
                        *curBuff = '\n';
                        ++curBuff;
                        break;
                    case 't':
                        *curBuff = '\t';
                        ++curBuff;
                        break;
                    case 'r':
                        *curBuff = '\r';
                        ++curBuff;
                        break;
                    default:
                        *curBuff = *curPos;
                        ++curBuff;
                        break;
                    }
                    ++ curPos;
                    continue;
                } else {
                    *curBuff = *curPos;
                    ++curBuff;
                    // take this character literally
                    ++curPos;
                    continue;
                }
            }
            if (0 == *curPos) {
                printf("error during tokenizing"
                       "string literal not terminated\n");
                return;
            }
            *curBuff = 0;
            // otherwise the string is tokenized
            assert( SMALL_BUFFER_SIZE >= (curBuff - buff + 1) );
            newTok = dynArrNew(tokenList);
            mkTokenString(newTok,buff);
            ++curPos;
        }
        tokenize(curPos,tokenList);
        return;
    }

    // dynamic initials
    if (isInitial(*curPos)) {
        // begin tokenizing symbol
        char *oldCurPos = curPos;
        ++curPos;
        // consume subsequent
        while (isSubsequent(*curPos))
            ++curPos;
        // print symbol
        strncpy(buff,oldCurPos,curPos-oldCurPos);
        buff[curPos-oldCurPos] = 0;
        newTok = dynArrNew(tokenList);
        mkTokenSymbol(newTok, buff);
        tokenize(curPos,tokenList);
        return;
    }
    if (isdigit(*curPos)) {
        // begin tokenizing number
        char *oldCurPos = curPos;
        ++curPos;
        // consume subsequent
        while (isdigit(*curPos))
            ++curPos;
        // print num
        strncpy(buff,oldCurPos,curPos-oldCurPos);
        buff[curPos-oldCurPos] = 0;
        newTok = dynArrNew(tokenList);
        mkTokenInteger(newTok,atol(buff));
        tokenize(curPos,tokenList);
        return;
    }
}