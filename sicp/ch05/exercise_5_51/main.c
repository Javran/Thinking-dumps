#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>

#include "evaluator.h"

char *srcText;
long srcSize;

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

    // static initials
    switch (*curPos) {
    case 0:
        // end of file
        printf("end of file\n");
        return;
    case ';':
        // a comment
        while ('\n' != *curPos && 0 != *curPos)
            ++curPos;
        printf("comment skipped\n");
        if (0 != *curPos) ++curPos;
        tokenize(curPos);
        return;
    case '(':
        printf("l paren\n");
        tokenize(++curPos);
        return;
    case ')':
        printf("r paren\n");
        tokenize(++curPos);
        return;
    case '\'':
        printf("quote symbol\n");
        tokenize(++curPos);
        return;
    case '#':
        ++curPos;
        // we will only handle "#t" and "#f" here ... for now
        switch (*curPos) {
        case 'T':
        case 't':
            printf("true value\n");
            tokenize(++curPos);
            return;
        case 'F':
        case 'f':
            printf("false value\n");
            tokenize(++curPos);
            return;
        default:
            printf("error: not supported\n");
            return;
        }
    case '"':
        ++curPos;
        {
            char *oldCurPos = curPos;
            // parse a string
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
            printf("string detected: %s\n", buff);
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
        printf("symbol detected: %s\n", buff);
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
        printf("number detected: %s\n", buff);
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
    tokenize(srcText);
    freeFile();

    tokenListInit();
    int i;
    for (i=0; i<100; ++i) {
        Token *tok = mkTokenEof();
        tokenListInsert(tok);
    }
    Token *p = tokenListBegin();
    for (; ! tokenListIsLastElement(p); ++p) {
        Token *p2 = p;
        freeToken(&p2);
    }
    tokenListFree();
    return 0;
}
