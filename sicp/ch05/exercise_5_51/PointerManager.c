#include "Common.h"
#include "DynArr.h"
#include "PointerManager.h"

// the point of PointerManager is to keep pointers allocated during
// the execution of the program.
// it is hard to keep track of every pointers when they get overwritten,
// so instead of checking and deallocating them as soon as possible,
// we just keep them in the memory and the deallocation is handled
// when the whole progam is done.
// this is not a good solution for a long-term running program,
// but just for this simple lisp interpreter, it should be fine.

// note that this implementation is not thread-safe because
// there is no guaranteed about the atomicity of DynArr, but I just
// feel passing the internal state of a pointer manager everywhere
// is annoying.

typedef DynArr PointerManagerState;

typedef struct {
    PHandle handle;
    PFreeCallback callback;
} PRecord;

DynArr pmState;

void pointerManagerInit() {
    memset(&pmState,0x00, sizeof(pmState));
    dynArrInit(&pmState, sizeof(PRecord));
}

void visitAndFree(PRecord *pr) {
    pr->callback(pr->handle);
}

void pointerManagerFinalize() {
    dynArrVisit(&pmState,(DynArrVisitor)visitAndFree);
    dynArrFree(&pmState);
    memset(&pmState,0x00, sizeof(pmState));
}

void pointerManagerRegisterCustom(PHandle h, PFreeCallback cb) {
    PRecord *pr = dynArrNew(&pmState);
    pr->handle = h;
    pr->callback = cb;
}

void pointerManagerRegister(PHandle h) {
    pointerManagerRegisterCustom(h,free);
}
