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

void envInit(Environment *env) {
    // all env fields should be zero.
    assert( env
            && !env->frame
            && !env->parent );
    env->frame = calloc(1, sizeof(Frame));
    frameInit(env->frame);
}

void envFree(Environment *env) {
    assert( env->frame );
    frameFree( env->frame );
    env->frame = NULL;
}
