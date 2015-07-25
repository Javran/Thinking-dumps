#include "FunctionObject.h"
#include "EvalSeq.h"
#include "PointerManager.h"

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

void releaseTempEnv(Environment *pEnv) {
    envFree(pEnv);
    free(pEnv);
}

// apply arguments to a function object
// this function assmes that all operands are evaluated
// thus it is not necessary to provide an environment
const SExp *funcObjApp(const FuncObj *rator, const SExp *rands) {
    // TODO:
    switch (rator->tag) {
    case funcPrim: {
        FuncPrimHandler handler = rator->fields.primHdlr;
        return handler(rands);
    }
    case funcCompound: {
        FuncCompound fc = rator->fields.compObj;
        const SExp *ps = fc.parameters;
        const SExp *bd = fc.body;
        Environment *env = fc.env;

        Environment *appEnv = calloc(1,sizeof(Environment));
        envInit(appEnv);
        pointerManagerRegisterCustom(appEnv, (PFreeCallback)releaseTempEnv);
        envSetParent(appEnv, env);

        while (sexpNil != rands->tag && sexpNil != ps->tag ) {
            if (sexpSymbol != sexpCar( ps )->tag)
                return NULL;
            char *varName = sexpCar( ps )->fields.symbolName;
            const SExp *rand = sexpCar( rands );
            envInsert(appEnv, varName, (void *)rand);
            rands = sexpCdr(rands);
            ps = sexpCdr(ps);
        }
        if (! (sexpNil == rands->tag && sexpNil == ps->tag) )
            return NULL;
        // execute body under new environment
        return evSequence(bd, appEnv);
    }
    }
    assert(0 /* invalid function object */);
}

void freeFuncObject(FuncObj *p) {
    // we only need to free the structure we are using.
    // just assume other components will be release properly.
    free(p);
}
