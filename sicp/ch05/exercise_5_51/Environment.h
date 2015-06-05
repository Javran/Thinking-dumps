#include "Common.h"
#include "Frame.h"

#ifndef _JAVEV_ENVIRONMENT_H_
#define _JAVEV_ENVIRONMENT_H_

struct Environment;

typedef struct Environment {
    // INVARIANT:
    // * frame is never NULL
    // * following parent recursively should
    //   not get stuck in a loop
    Frame *frame;
    const struct Environment *parent;
} Environment;

void envInit(Environment *);
void envSetParent(Environment *, const Environment *);
FrameEntry *envLookup(const Environment *, const char *);
void envInsert(Environment *, const char *, void *);
void envFree(Environment *);

#endif
