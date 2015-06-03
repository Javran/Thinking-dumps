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

FrameEntry *frameLookup(Frame *frame, const char *keyword) {
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

void *frameInsert(Frame *frame, const char *key, void *val) {
    void *retVal = NULL;
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

// TODO: current problem: need proper handling for values' free
// need to provide an interface for value-visiting or key-value-visiting
void frameEntryFree(FrameEntry *fe) {
    free(fe->key);
}

void frameFree(Frame *frame) {
    dynArrVisit(frame,(DynArrVisitor)frameEntryFree);
    dynArrFree(frame);
}
