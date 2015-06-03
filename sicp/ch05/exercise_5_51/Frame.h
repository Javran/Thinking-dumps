#include "DynArr.h"

#ifndef _JAVEV_FRAME_H_
#define _JAVEV_FRAME_H_

typedef struct {
    char *key;
    void *val;
} FrameEntry;

typedef DynArr Frame;

void frameInit(DynArr *);
FrameEntry *frameLookup(Frame *, const char *);
void *frameInsert(Frame *, const char *, void *);
void frameFree(Frame *);

#endif
