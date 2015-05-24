#include <stdlib.h>

#ifndef _JAVEV_DYNARR_H_
#define _JAVEV_DYNARR_H_

typedef void (*DynArrVisitor)(void *);

typedef struct {
    // base pointer for this dynamic array
    unsigned char *base;
    // size of each element
    // should always be a result of sizeof operator
    // or we might get into alignment-related troubles
    size_t elemSize;
    // number of elements currently have
    size_t elemMax;
    // current capacity
    size_t elemCap;
} DynArr;

void dynArrInit(DynArr *, size_t);
void *dynArrNew(DynArr *);
void dynArrFree(DynArr *);
void *dynArrBegin(DynArr *);
void *dynArrLast(DynArr *);
void *dynArrEnd(DynArr *);
void *dynArrNext(DynArr *, void *);
int dynArrCount(DynArr *);
void dynArrVisit(DynArr *, DynArrVisitor);

#endif
