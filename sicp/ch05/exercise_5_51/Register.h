#include "SExp.h"
#include "Environment.h"

#ifndef _JAVEV_REGISTER_H_
#define _JAVEV_REGISTER_H_

// TODO: Register -> EvValue when
// the Machine can be totally removed.

typedef enum {
    // use this value when the return value
    // should be unspecified
    regUnspecified,
    regBool,
    regSExp,
    regEnvironment,
    regLamda,
} RegTag;

typedef union {
    // INVARIANT: truthValue = either 0 or 1
    char truthValue;
    const SExp *asSExp;
    Environment *asEnv;
    // quick and dirty
    void *asLambda;
} RegContent;

typedef struct {
    RegTag tag;
    RegContent data;
} Register;

#endif
