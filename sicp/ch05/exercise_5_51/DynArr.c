// a simple self-adjusting array

#include <assert.h>
#include <stdlib.h>
#include <string.h>

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

void dynArrFree(DynArr *p) {
    assert( p );
    free(p->base);
    memset(p, 0x00, sizeof(DynArr));
}
