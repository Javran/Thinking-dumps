#include "Common.h"
#include "Syntax.h"
#include "Token.h"
#include "Tokenizer.h"

void tokenize(const char *curPos, DynArr *tokenList, FILE* err) {
    // INVARIANT:
    // * every possible path does an explicit tail recursive call
    //   unless EOF is reached
    // * if something is not supported, a EOF token will be generated
    //   in the result list and an error message outputed to err
    // * result tokenList is always non-empty

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
        tokenize(curPos,tokenList,err);
        return;
    case '(':
        newTok = dynArrNew(tokenList);
        mkTokenLParen(newTok);
        tokenize(++curPos,tokenList,err);
        return;
    case ')':
        newTok = dynArrNew(tokenList);
        mkTokenRParen(newTok);
        tokenize(++curPos,tokenList,err);
        return;
    case '\'':
        newTok = dynArrNew(tokenList);
        mkTokenQuote(newTok);
        tokenize(++curPos,tokenList,err);
        return;
    case '#':
        ++curPos;
        // we will only handle "#t" and "#f" here ... for now
        switch (*curPos) {
        case 'T':
        case 't':
            newTok = dynArrNew(tokenList);
            mkTokenTrue(newTok);
            tokenize(++curPos,tokenList,err);
            return;
        case 'F':
        case 'f':
            newTok = dynArrNew(tokenList);
            mkTokenFalse(newTok);
            tokenize(++curPos,tokenList,err);
            return;
        default:
            goto failed;
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
        tokenize(curPos,tokenList,err);
        return;
    }

    // dynamic initials
    if (isInitial(*curPos)) {
        // begin tokenizing symbol
        const char *oldCurPos = curPos;
        ++curPos;
        // consume subsequent
        while (isSubsequent(*curPos))
            ++curPos;
        // print symbol
        strncpy(buff,oldCurPos,curPos-oldCurPos);
        buff[curPos-oldCurPos] = 0;
        newTok = dynArrNew(tokenList);
        mkTokenSymbol(newTok, buff);
        tokenize(curPos,tokenList,err);
        return;
    }
    if (isdigit(*curPos)) {
        // begin tokenizing number
        const char *oldCurPos = curPos;
        ++curPos;
        // consume subsequent
        while (isdigit(*curPos))
            ++curPos;
        // print num
        strncpy(buff,oldCurPos,curPos-oldCurPos);
        buff[curPos-oldCurPos] = 0;
        newTok = dynArrNew(tokenList);
        mkTokenInteger(newTok,atol(buff));
        tokenize(curPos,tokenList,err);
        return;
    }

failed:
    // if something goes wrong during tokenization,
    // we report the error and terminate tokenization
    // but at the same time we create a EOF token
    // so that we can make sure the result list is always non-empty
    fprintf(err,"error: unsupported char at %d\n",(int)(curPos - buff));
    // end of file
    newTok = dynArrNew(tokenList);
    mkTokenEof(newTok);
    return;
}
