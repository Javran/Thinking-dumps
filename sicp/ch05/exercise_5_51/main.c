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

DynArr gTokenList;
ParseState gParseState;
DynArr gSExpList; // a list of (SExp *)

char *loadFile(const char* fileName) {
    FILE* fp = fopen(fileName,"r");
    // find file length
    fseek(fp,0,SEEK_END);
    size_t srcSize = ftell(fp);
    fseek(fp,0,SEEK_SET);

    // read into memory
    // one extra bit for string termination
    char *retVal = malloc(srcSize+1);
    fread(retVal, 1, srcSize, fp);
    retVal[srcSize] = 0;
    fclose(fp);
    return retVal;
}

void helpAndQuit(char *execName) {
    fprintf(stderr,
            "Usage: %s <lisp source file>\n",
            execName);
    exit(1);
}

void printAndFreeSExpP(SExp **p) {
    printSExp(stdout,*p); puts("");
    freeSExp(*p);
}

void freeSExpP(SExp **p) {
    freeSExp(*p);
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

    char *srcText = loadFile( fileName );
    dynArrInit(&gTokenList, sizeof(Token));
    dynArrInit(&gSExpList, sizeof(SExp *));

    tokenize(srcText,&gTokenList);
    free( srcText ); srcText = NULL;

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

    // keep parsing results until there is an error
    // since there is no handler for tokEof,
    // an error must happen, which guarantees that
    // this loop can terminate.
    for (result = parseSExp(&gParseState);
         NULL != result;
         result = parseSExp(&gParseState)) {
        SExp **newExp = dynArrNew(&gSExpList);
        *newExp = result;
    }

    // it is guaranteed that parseStateCurrent always produces
    // a valid pointer. no check is necessary.
    char parseFailed = ! ( tokEof == parseStateCurrent(&gParseState)->tag );
    if (parseFailed) {
        printf("Remaining tokens:\n");
        while (parseStateLookahead(&gParseState)) {
            printToken(stdout, parseStateCurrent(&gParseState) );
            parseStateNext(&gParseState);
        }
        putchar('\n');
    } else {
        // now we are ready for interpreting these s-expressions
    }

    // releasing resources
    dynArrVisit(&gSExpList,(DynArrVisitor)freeSExpP);
    dynArrVisit(&gTokenList,(DynArrVisitor)freeToken);

    dynArrFree(&gSExpList);
    dynArrFree(&gTokenList);

    return parseFailed ? EXIT_FAILURE : EXIT_SUCCESS;
}
