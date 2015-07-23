#include "FunctionObject.h"

FuncObj *newCompoundFunc(const SExp *p,
                              const SExp *bd,
                              Environment *e) {
    FuncObj *fc = calloc(1, sizeof(FuncCompound));
    fc->tag = funcCompound;
    fc->fields.compObj.parameters = p;
    fc->fields.compObj.body = bd;
    fc->fields.compObj.env = e;
    return fc;
}

void freeFuncObject(FuncObj *p) {
    free(p);
}
