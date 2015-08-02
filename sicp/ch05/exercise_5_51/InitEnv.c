#include "InitEnv.h"

// InitEnv.c is tested together with Primitives.c
// see PrimitivesTests.c under tests for details

Environment *mkInitEnv() {
    Environment *env = calloc(1, sizeof(Environment));
    // parent environment is implictily set to NULL
    envInit(env);
    envInsert(env, "+", &primPlusSExp);
    envInsert(env, "-", &primMinusSExp);
    envInsert(env, "*", &primMultSExp);
    return env;
}
