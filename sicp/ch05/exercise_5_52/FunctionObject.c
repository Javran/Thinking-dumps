#include "FunctionObject.h"

FuncObj *newCompoundFunc(const SExp *p,
                              const SExp *bd,
                              Environment *e) {
    FuncObj *fc = calloc(1, sizeof(FuncObj));
    fc->tag = funcCompound;
    fc->fields.compObj.parameters = p;
    fc->fields.compObj.body = bd;
    fc->fields.compObj.env = e;
    return fc;
}

void releaseTempEnv(Environment *pEnv) {
    envFree(pEnv);
    free(pEnv);
}

void freeFuncObject(FuncObj *p) {
    // we only need to free the structure we are using.
    // just assume other components will be released properly.
    free(p);
}
