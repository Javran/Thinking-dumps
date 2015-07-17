#ifndef JAVEV_ENVIRONMENT_H
#define JAVEV_ENVIRONMENT_H

#include "Common.h"
#include "Frame.h"

struct Environment;

typedef struct Environment {
    // INVARIANT:
    // * frame is never NULL
    // * following parent recursively should
    //   not get stuck in a loop
    Frame *frame;
    const struct Environment *parent;
} Environment;

// TODO: value should be of type SExp *,
// if everything else works out fine

void envInit(Environment *);
void envSetParent(Environment *, const Environment *);
FrameEntry *envLookup(const Environment *, const char *);
void envInsert(Environment *, const char *, void *);
void envFree(Environment *);

#endif
