#include "Environment.h"
#include "Primitives.h"

Environment *mkInitEnv() {
    Environment *env = calloc(1, sizeof(Environment));
    envInit(env);
    envInsert(env, "+", NULL);
    return env;
}
