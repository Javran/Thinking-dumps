#ifndef _JAVEV_DYNARR_H_
#define _JAVEV_DYNARR_H_

#include "Common.h"

typedef void (*DynArrVisitor)(void *);
typedef void * (*DynArrFoldLeftAccumulator)(void *, void *);

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
void *dynArrNth(const DynArr *, int);
void *dynArrBegin(const DynArr *);
void *dynArrLast(const DynArr *);
void *dynArrEnd(const DynArr *);
void *dynArrNext(const DynArr *, void *);
int dynArrCount(const DynArr *);
void dynArrVisit(DynArr *, DynArrVisitor);

void * dynArrFoldLeft(DynArr *, DynArrFoldLeftAccumulator, void *);

#endif
