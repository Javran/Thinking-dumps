#include "InitEnv.h"

Environment *mkInitEnv() {
    Environment *env = calloc(1, sizeof(Environment));
    // parent environment is implictily set to NULL
    envInit(env);
    envInsert(env, "+", &primPlusSExp);
    return env;
}
