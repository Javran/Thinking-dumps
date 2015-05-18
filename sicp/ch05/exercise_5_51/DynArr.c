// a simple self-adjusting array

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#include "evaluator.h"

// TODO: about free:
// so far I'm coding but haven't make
// any principle about coding in C.
// one rule I have to set up here is about
// the responsibility of freeing allocated objects,
// let's just say:
// * a procedure that frees an object, is only responsible
//   for freeing the objects' reachable inner structures.
//   and the responsiblity of freeing the object itself
//   should be taken care by the caller to this free procedure.

void dynArrInit(DynArr *p, size_t elemSize) {
    // pointer should not be null
    // all fields should be zero
    assert( p
            && !p->base
            && !p->elemSize
            && !p->elemMax
            && !p->elemCap );
    p->elemMax = 0;
    p->elemSize = elemSize;
    p->elemCap = SMALL_BUFFER_SIZE / elemSize;
    p->base = calloc(elemSize, p->elemCap);
}

// adjust the array so that it guarantees
// to contain the next element
// for internal use. no need to expose it
void dynArrAdjust(DynArr *p) {
    // should not be zero
    assert(p->elemCap);
    if (p->elemMax+1 >= p->elemCap) {
        p->elemCap *= 2;
        p->base = realloc(p->base, p->elemSize*p->elemCap);
        assert( p->base );
    }
}

// allocate a new object
void *dynArrNew(DynArr *p) {
    dynArrAdjust(p);
    void *retVal = p->base + p->elemMax*p->elemSize;
    ++ p->elemMax;
    return retVal;
}

void dynArrFree(DynArr *p) {
    assert( p );
    free(p->base);
    memset(p, 0x00, sizeof(DynArr));
}

// return the place pointing to the first element
void *dynArrBegin(DynArr *p) {
    return p->base;
}

// return the last element, might not be a valid
// pointer depending on the emptiness.
void *dynArrLast(DynArr *p) {
    return p->base + p->elemSize*(p->elemMax - 1);
}

void *dynArrEnd(DynArr *p) {
    return p->base + p->elemSize*p->elemMax;
}

// get next pointer
void *dynArrNext(DynArr *p, void *ptr) {
    unsigned char *tPtr = ptr;
    return tPtr + p->elemSize;
}
