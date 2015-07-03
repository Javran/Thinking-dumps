#ifndef _JAVEV_REGISTER_H_
#define _JAVEV_REGISTER_H_

#include "SExp.h"
#include "Environment.h"

// TODO: Register -> EvValue when
// the Machine can be totally removed.

// TODO: The result should actually be of type "SExp *".

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
