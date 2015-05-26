#include "Common.h"
#include "Token.h"
#include "DynArr.h"
#include "Tokenizer.h"
#include "Parser.h"
#include "Util.h"

char *srcText;
long srcSize;
DynArr gTokenList;
ParseState gParseState;

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

    tokenize(srcText,&gTokenList);

    if (dynArrCount(&gTokenList)) {
        // TODO: call parser here
        // to make sure we have at least one token to worry about.
        memset(&gParseState, 0x00, sizeof(gParseState));
        parseStateInit(&gTokenList,&gParseState);

        SExp *result = parseSExp(&gParseState);
        (void)result;

        while (parseStateLookahead(&gParseState)) {
            printToken(stdout, parseStateCurrent(&gParseState) );
            parseStateNext(&gParseState);
        }

    } else {
        fprintf(stderr, "Empty token list.\n");
    }

    freeFile();

    dynArrVisit(&gTokenList,(DynArrVisitor)freeToken);
    dynArrFree(&gTokenList);
    return 0;
}
