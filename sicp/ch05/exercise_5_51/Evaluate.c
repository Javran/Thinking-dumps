#include "PointerManager.h"
#include "Evaluate.h"

// TODO: eventually we will replace Machine by Evaluate
// eliminating the simulated machine in favor of the built-in
// abstract machine of c language itself
SExp *evalProgramText(const char *programText, FILE *errF) {
    DynArr *pSExpList = parseSExps(programText, errF);
    // it is guaranteed that parseStateCurrent always produces
    // a valid pointer. no check is necessary.
    char parseFailed = !pSExpList;
    if (!parseFailed) {
        // now we are ready for interpreting these s-expressions
        pointerManagerInit();


        pointerManagerFinalize();
    }
    // releasing resources
    freeSExps(pSExpList);
    return NULL;
}
