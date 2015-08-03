#include "FunctionObject.h"
#include "Evaluate.h"
#include "EvalSeq.h"
#include "PointerManager.h"

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

// apply arguments to a function object
const SExp *funcObjApp(const FuncObj *rator, const SExp *rands, Environment *env) {
    assert(rator /* operator cannot be NULL */);
    switch (rator->tag) {
    case funcPrim: {
        FuncPrimHandler handler = rator->fields.primHdlr;
        // TODO: nil assumption?
        const SExp *randVals = newNil();
        while (sexpNil != rands->tag) {
            const SExp *carE = sexpCar( rands );
            const SExp *cdrE = sexpCdr( rands );
            const SExp *carV = evalDispatch(carE, env);
            if (! carV) return NULL;
            // turns out evaluating things from left to right will be tricky
            assert( 0 );
        }
        // TODO: we need to evaluate these operands here.
        return handler(rands);
    }
    case funcCompound: {
        FuncCompound fc = rator->fields.compObj;
        const SExp *ps = fc.parameters;
        const SExp *bd = fc.body;
        Environment *fenv = fc.env;

        Environment *appEnv = calloc(1,sizeof(Environment));
        envInit(appEnv);
        pointerManagerRegisterCustom(appEnv, (PFreeCallback)releaseTempEnv);
        envSetParent(appEnv, fenv);

        while (sexpNil != rands->tag && sexpNil != ps->tag ) {
            if (sexpSymbol != sexpCar( ps )->tag)
                return NULL;
            char *varName = sexpCar( ps )->fields.symbolName;
            const SExp *rand = sexpCar( rands );
            const SExp *randVal = evalDispatch(rand, env);
            if (!randVal) return NULL;
            envInsert(appEnv, varName, (void *)randVal);
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
