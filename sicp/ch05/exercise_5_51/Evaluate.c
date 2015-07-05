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

        const SExp *result = NULL;
        // TODO: proper env initialization
        Environment *env = NULL;
        void * it;
        for (it = dynArrBegin(pSExpList);
             it != dynArrEnd(pSExpList);
             it = dynArrNext(pSExpList, it)) {
            result = evalDispatch1((SExp *)it, env);
            if (!result) {
                // TODO:
                // evaluation failed, stop evaluation at this point
                // maybe giving more messages will be better
                break;
            }

        }
        pointerManagerFinalize();
    }
    // releasing resources
    freeSExps(pSExpList);
    return NULL;
}
