#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>

#include "evaluator.h"

char *srcText;
long srcSize;
DynArr tokenList;

void loadFile(const char* fileName) {
    FILE* fp = fopen(fileName,"r");
    // find file length
    fseek(fp,0,SEEK_END);
    srcSize = ftell(fp);
    fseek(fp,0,SEEK_SET);

    // read into memory
    // one extra bit for string termination
    srcText = malloc(srcSize+1);
    fread(srcText, 1, srcSize, fp);
    srcText[srcSize] = 0;
    fclose(fp);
}

void freeFile() {
    free(srcText);
    srcText = NULL;
}

void tokenize(char *curPos) {
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
        newTok = dynArrNew(&tokenList);
        mkTokenEof(newTok);
        return;
    case ';':
        // a comment
        while ('\n' != *curPos && 0 != *curPos)
            ++curPos;
        if (0 != *curPos) ++curPos;
        tokenize(curPos);
        return;
    case '(':
        newTok = dynArrNew(&tokenList);
        mkTokenLParen(newTok);
        tokenize(++curPos);
        return;
    case ')':
        newTok = dynArrNew(&tokenList);
        mkTokenRParen(newTok);
        tokenize(++curPos);
        return;
    case '\'':
        newTok = dynArrNew(&tokenList);
        mkTokenQuote(newTok);
        tokenize(++curPos);
        return;
    case '#':
        ++curPos;
        // we will only handle "#t" and "#f" here ... for now
        switch (*curPos) {
        case 'T':
        case 't':
            newTok = dynArrNew(&tokenList);
            mkTokenTrue(newTok);
            return;
        case 'F':
        case 'f':
            newTok = dynArrNew(&tokenList);
            mkTokenFalse(newTok);
            return;
        default:
            fprintf(stderr,"error: not supported\n");
            return;
        }
    case '"':
        ++curPos;
        {
            char *oldCurPos = curPos;
            // parse a string
            // TODO: for proper handling,
            // we should keep a counter for buffer
            while (0 != *curPos && '"' != *curPos) {
                if ('\\' == *curPos) {
                    ++curPos;
                    // handle escaping
                    switch (*curPos) {
                    case 'n':
                        printf("newline\n");
                        break;
                    case 't':
                        printf("tab\n");
                        break;
                    case 'r':
                        printf("carry\n");
                        break;
                    default:
                        // take this character literally
                        break;
                    }
                    ++ curPos;
                    continue;
                } else {
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
            // otherwise the string is tokenized
            assert( SMALL_BUFFER_SIZE >= (curPos - oldCurPos + 1) );
            strncpy(buff,oldCurPos,curPos - oldCurPos);
            buff[curPos-oldCurPos] = 0;
            // newTok = dynArrNew(&tokenList);
            // mkTokenString(newTok,buff);
            ++curPos;
        }
        tokenize(curPos);
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
        // printf("symbol detected: %s\n", buff);
        // newTok = dynArrNew(&tokenList);
        // mkTokenSymbol(newTok, buff);
        tokenize(curPos);
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
        //newTok = dynArrNew(&tokenList);
        //mkTokenInteger(newTok,atol(buff));
        tokenize(curPos);
        return;
    }
}

int main(int argc, char *argv[]) {
    // just assume we have only one argument,
    // which is the file
    assert( argc == 1+1 );
    loadFile( argv[1] );
    // TODO: second pass tokenize
    // TODO: first we do tokenizing without storing anything.
    dynArrInit(&tokenList, sizeof(Token));

    tokenize(srcText);
    freeFile();

    Token *it; int count = 0;
    for (it = dynArrBegin(&tokenList);
         it != dynArrEnd(&tokenList);
         it = dynArrNext(&tokenList,it)) {
        ++count;
        freeToken(it);
    }
    printf("token count=%d\n",count);
    dynArrFree(&tokenList);
    return 0;
}
