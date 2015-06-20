#include "SExp.h"
#include "Environment.h"

#ifndef _JAVEV_REGISTER_H_
#define _JAVEV_REGISTER_H_

typedef enum {
    regBool,
    regSExp,
    regEnvironment,
    regLamda,
} RegTag;

typedef union {
    // INVARIANT: truthValue = either 0 or 1
    const char truthValue;
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
