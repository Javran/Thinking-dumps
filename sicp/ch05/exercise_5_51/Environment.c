#include "Environment.h"

// note that NULL is treated as if it was an empty environment
// the empty environment is special because no insertion is allowed.
// a consequence follows that, without setting the parent environment explicitly,
// every runtime-created environment's parent is the empty environment.

void envInit(Environment *env) {
    // all env fields should be zero.
    assert( env
            && !env->frame
            && !env->parent );
    env->frame = calloc(1, sizeof(Frame));
    frameInit(env->frame);
}

// set or unset parent
void envSetParent(Environment *env, const Environment *parent) {
    env->parent = parent;
}

FrameEntry *envLookup(const Environment *env, const char *keyword) {
    if (NULL == env) return NULL;
    FrameEntry *result = frameLookup(env->frame,keyword);
    return result
        ? result
        : envLookup(env->parent, keyword);
}

void envInsert(Environment *env, const char *key, void *val) {
    assert( env /* cannot insert binding into the empty environment */);
    frameInsert(env->frame,key,val);
}

void envFree(Environment *env) {
    if (env) {
        assert( env->frame );
        frameFree( env->frame );
        free( env->frame );
        env->frame = NULL;
    }
}
