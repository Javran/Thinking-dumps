#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <string.h>

#define SMALL_BUFFER_SIZE 512

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

int isLetter(char c) { return isalpha(c); }

int isSpecialInitial(char c) {
    static const char *sp = "!$%&*/:<=>?^_~";
    const int spLen = strlen(sp);
    int i;
    for (i=0; i<spLen; ++i)
        if (sp[i] == c)
            return 1;
    return 0;
}

int isPeculiarIdentifierInitial(char c) {
    // I don't want to worry about "..." here
    return '+' == c || '-' == c;
}

int isSpecialSubsequent(char c) {
    return '+' == c || '-' == c
        || '.' == c || '@' == c;
}

int isInitial(char c) {
 return isLetter(c)
     || isSpecialInitial(c)
     || isPeculiarIdentifierInitial(c);
}

int isSubsequent(char c) {
    return isInitial(c)
        || isdigit(c)
        || isSpecialSubsequent(c);
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
    return 0;
}
