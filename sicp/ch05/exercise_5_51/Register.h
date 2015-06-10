#include "SExp.h"
#include "Environment.h"

#ifndef _JAVEV_REGISTER_H_
#define _JAVEV_REGISTER_H_

typedef enum {
    regBool,
    regSExp,
    regEnvironment,
} RegTag;

typedef union {
    // INVARIANT: truthValue = either 0 or 1
    char truthValue;
    SExp *asSExp;
    Environment *asEnv;
} RegContent;

typedef struct {
    RegTag tag;
    RegContent data;
} Register;

#endif
