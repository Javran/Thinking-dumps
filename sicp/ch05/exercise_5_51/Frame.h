#ifndef JAVEV_FRAME_H
#define JAVEV_FRAME_H

#include "DynArr.h"
#include "SExp.h"

typedef struct {
    char *key;
    const SExp *val;
} FrameEntry;

typedef DynArr Frame;

void frameInit(DynArr *);
FrameEntry *frameLookup(const Frame *, const char *);
const SExp *frameInsert(Frame *, const char *, const SExp *);
void frameFree(Frame *);

#endif
