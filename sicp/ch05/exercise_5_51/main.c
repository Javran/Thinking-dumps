#include "Common.h"
#include "Token.h"
#include "DynArr.h"
#include "Tokenizer.h"
#include "Parser.h"
#include "Util.h"

// TODO: consider this to be just a quick and dirty
// evaluator, plus some experimental idea of optimization.
// should we deal with error cases?
// I'm a little bit worrying that handling errors would
// complicate the code

char *srcText;
long srcSize;
DynArr gTokenList;
ParseState gParseState;
DynArr gSExpList; // a list of (SExp *)

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

void helpAndQuit(char *execName) {
    fprintf(stderr,
            "Usage: %s <lisp source file>\n",
            execName);
    exit(1);
}

int main(int argc, char *argv[]) {
    if ( argc != 1+1 )
        helpAndQuit(argv[0]);

    const char *fileName = argv[1];
    if ( !isFileExist(fileName) ||
         !isFileReadable(fileName) ) {
        fprintf(stderr,
                "File is not available.\n");
        exit(errno);
    }

    loadFile( fileName );
    dynArrInit(&gTokenList, sizeof(Token));
    dynArrInit(&gSExpList, sizeof(SExp *));

    tokenize(srcText,&gTokenList);

    // TODO: have a list of `SExp*`.
    // stop when the parse produces nothing

    assert( dynArrCount(&gTokenList)
            /* the tokenizer should at least return tokEof,
               making the token list non-empty
             */);
    // at this point
    // we have at least one element in the token list,
    // which is the invariant we need to maintain
    // when calling parser.
    memset(&gParseState, 0x00, sizeof(gParseState));
    parseStateInit(&gTokenList,&gParseState);


    SExp *result = NULL;

    for (result = parseSExp(&gParseState);
         NULL != result;
         result = parseSExp(&gParseState)) {
        printSExp(stdout,result); puts("");
        freeSExp(result);
    }

    printf("Remainng tokens:\n");
    // TODO: should have nothing to consume now
    while (parseStateLookahead(&gParseState)) {
        printToken(stdout, parseStateCurrent(&gParseState) );
        parseStateNext(&gParseState);
    }

    freeFile();

    dynArrVisit(&gTokenList,(DynArrVisitor)freeToken);

    dynArrFree(&gSExpList);
    dynArrFree(&gTokenList);
    return 0;
}
