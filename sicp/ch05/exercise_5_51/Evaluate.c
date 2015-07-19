#include "PointerManager.h"
#include "Evaluate.h"

#include "Common.h"
#include "SExp.h"
#include "Environment.h"

#include "EvalSimple.h"
#include "EvalCond.h"
#include "EvalSeq.h"
#include "EvalSetDefine.h"

SExpHandler *evalHandlers[] = {
    &selfEvaluatingHandler,
    &variableHandler,
    &quotedHandler,
    &assignmentHandler,
    &definitionHandler,
    &ifHandler,
    &lambdaHandler,
    &beginHandler
    // TODO: application missing
};

const size_t evalHandlerCount =
    sizeof(evalHandlers) / sizeof(SExpHandler *);

// TODO:
// * application

const SExp *evalDispatch(const SExp *exp, Environment *env) {
    // INVARIANT: every branch should end with a return
    size_t i;
    for (i=0; i<evalHandlerCount; ++i) {
        SExpHandler *h = evalHandlers[i];
        if (h->pred(exp))
            return h->eval(exp,env);
    }
    return NULL;
}

SExp *evalProgramText(const char *programText, FILE *errF) {
    DynArr *pSExpList = parseSExps(programText, errF);
    // it is guaranteed that parseStateCurrent always produces
    // a valid pointer. no check is necessary.
    char parseFailed = !pSExpList;
    if (!parseFailed) {
        // now we are ready for interpreting these s-expressions
        pointerManagerInit();

        const SExp *result = NULL;
        // TODO: proper env initialization
        Environment *env = NULL;
        SExp ** it;
        for (it = dynArrBegin(pSExpList);
             it != dynArrEnd(pSExpList);
             it = dynArrNext(pSExpList, it)) {
            result = evalDispatch(*it, env);
            if (!result) {
                // TODO:
                // evaluation failed, stop evaluation at this point
                // maybe giving more messages will be better
                break;
            }

        }
        pointerManagerFinalize();
    }
    // releasing resources
    freeSExps(pSExpList);
    return NULL;
}
