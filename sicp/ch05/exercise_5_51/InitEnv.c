#include "InitEnv.h"

Environment *mkInitEnv() {
    Environment *env = calloc(1, sizeof(Environment));
    envInit(env);
    envInsert(env, "+", &primPlusSExp);
    return env;
}
