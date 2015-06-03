#include "Common.h"
#include "Frame.h"

struct Environment;

typedef struct Environment {
    // INVARIANT:
    // * frame is never NULL
    // * following parent recursively should
    //   not get stuck in a loop
    Frame *frame;
    struct Environment *parent;
} Environment;
