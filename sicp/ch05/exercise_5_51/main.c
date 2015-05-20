#include "Common.h"
#include "Token.h"
#include "DynArr.h"
#include "Tokenizer.h"

char *srcText;
long srcSize;
DynArr gTokenList;

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

int main(int argc, char *argv[]) {
    // TODO: better error handling
    // just assume we have only one argument,
    // which is the file
    assert( argc == 1+1 );
    loadFile( argv[1] );
    dynArrInit(&gTokenList, sizeof(Token));

    tokenize(srcText,&gTokenList);
    freeFile();

    dynArrVisit(&gTokenList,(DynArrVisitor)freeToken);
    dynArrFree(&gTokenList);
    return 0;
}
