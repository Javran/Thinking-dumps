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

    envInsert(env, "cons", &primConsSExp);
    envInsert(env, "car", &primCarSExp);
    envInsert(env, "cdr", &primCdrSExp);
    envInsert(env, "list", &primListSExp);

    envInsert(env, "symbol?", &primSymbolQSExp);
    envInsert(env, "string?", &primStringQSExp);
    envInsert(env, "integer?", &primIntegerQSExp);
    envInsert(env, "boolean?", &primBooleanQSExp);
    envInsert(env, "null?", &primNullQSExp);
    envInsert(env, "pair?", &primPairQSExp);

    envInsert(env, "=", &primEQSExp);
    // TODO: test
    envInsert(env, "eq?", &primEqQSExp);

    envInsert(env, "not", &primNotSExp);

    return env;
}
