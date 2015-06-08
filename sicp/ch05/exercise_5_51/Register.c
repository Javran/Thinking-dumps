#include "Common.h"
#include "SExp.h"
#include "Environment.h"

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
