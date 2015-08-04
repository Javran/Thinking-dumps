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

// (internal use only) turning argument expressions into values
// given that application itself is proper list, randExps is always
// either nil or pair
const SExp *evalArgs(const SExp *randExps, Environment *env) {
    if (sexpNil == randExps->tag)
        return newNil();

    const SExp *firstVal = evalDispatch(sexpCar(randExps), env);
    const SExp *restVals = evalArgs(sexpCdr(randExps), env);

    // TODO: eliminate hacks
    return newPair((void *)firstVal,(void *)restVals);
}

// apply arguments to a function object
const SExp *funcObjApp(const FuncObj *rator, const SExp *rands, Environment *env) {
    assert(rator /* operator cannot be NULL */);
    switch (rator->tag) {
    case funcPrim: {
        FuncPrimHandler handler = rator->fields.primHdlr;
        const SExp *randVals = evalArgs(rands,env);
        // TODO: remove hack
        // pointerManagerRegisterCustom((void *)randVals, (PFreeCallback)freeSExp);
        return handler(randVals);
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
