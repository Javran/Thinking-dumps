#include "SExp.h"
#include "Environment.h"

#ifndef _JAVEV_REGISTER_H_
#define _JAVEV_REGISTER_H_

typedef enum {
    regSExp,
    regEnvironment,
} RegTag;

typedef union {
    SExp *asSExp;
    Environment *asEnv;
} RegContent;

typedef struct {
    RegTag tag;
    RegContent data;
} Register;

#endif
