#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>

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
        printf("l paren");
        tokenize(++curPos);
        return;
    case ')':
        printf("r paren");
        tokenize(++curPos);
        return;
    }

    // dynamic initials

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
