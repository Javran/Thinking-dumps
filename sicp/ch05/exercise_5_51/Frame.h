#ifndef _JAVEV_FRAME_H_
#define _JAVEV_FRAME_H_

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
