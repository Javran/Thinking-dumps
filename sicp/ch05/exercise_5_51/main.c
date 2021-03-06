#include "Common.h"
#include "Evaluate.h"
#include "Util.h"

// here we keep the convention that returning NULL means error
// it is recommended but optional to print out information which
// could come in handy for troubleshooting.

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
    char result = evalProgramText(srcText, stderr);
    free(srcText); srcText = NULL;

    return result ? EXIT_SUCCESS : EXIT_FAILURE;
}
