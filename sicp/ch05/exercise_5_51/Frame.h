#ifndef JAVEV_FRAME_H
#define JAVEV_FRAME_H

#include "DynArr.h"

typedef struct {
    char *key;
    void *val;
} FrameEntry;

typedef DynArr Frame;

void frameInit(DynArr *);
FrameEntry *frameLookup(const Frame *, const char *);
void *frameInsert(Frame *, const char *, void *);
void frameFree(Frame *);

#endif
