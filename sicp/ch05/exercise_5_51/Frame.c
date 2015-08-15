// a frame contains environment bindings,
// * keys are unique
// * deletion is not allowed
// therefore ... we can use DynArr
// search functionality can be handled through a left-fold

#include "Common.h"
#include "Util.h"
#include "Frame.h"

void frameInit(DynArr *pairs) {
    dynArrInit(pairs, sizeof(FrameEntry));
}

FrameEntry *frameLookup(const Frame *frame, const char *keyword) {
    FrameEntry *result = NULL;
    FrameEntry *it;
    for (it = dynArrBegin(frame);
         !result && it != dynArrEnd(frame);
         it = dynArrNext(frame,it))
        if (0 == strcmp(it->key, keyword)) {
            return it;
        }
    return NULL;
}

const SExp *frameInsert(Frame *frame, const char *key, const SExp *val) {
    const SExp *retVal = NULL;
    FrameEntry *fe = frameLookup(frame, key);
    if (fe) {
        // an old object is found
        retVal = fe->val;
        fe->val = val;
    } else {
        // need to insert a new object
        fe = dynArrNew(frame);
        fe->key = allocCopyString(key);
        fe->val = val;
    }
    return retVal;
}

// here we just free whatever we have allocated,
// as values are allocated by other functions
// and there is already a mechanism for deallocating them
void frameEntryFree(FrameEntry *fe) {
    free(fe->key);
}

void frameFree(Frame *frame) {
    dynArrVisit(frame,(DynArrVisitor)frameEntryFree);
    dynArrFree(frame);
}
