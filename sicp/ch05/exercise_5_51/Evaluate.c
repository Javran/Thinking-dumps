#include "PointerManager.h"
#include "Evaluate.h"

#include "Common.h"
#include "SExp.h"
#include "Environment.h"

#include "EvalSimple.h"
#include "EvalCond.h"
#include "EvalSeq.h"
#include "EvalSetDefine.h"
#include "EvalApp.h"

#include "InitEnv.h"

SExpHandler *evalHandlers[] = {
    &selfEvaluatingHandler,
    &variableHandler,
    &quotedHandler,
    &assignmentHandler,
    &definitionHandler,
    &ifHandler,
    &lambdaHandler,
    &beginHandler,
    &applicationHandler
};

const size_t evalHandlerCount =
    sizeof(evalHandlers) / sizeof(SExpHandler *);

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
        // record how many s-exprs are evaluated successfully
        int successCnt = 0;
        Environment *env = mkInitEnv();
        SExp ** it;
        for (it = dynArrBegin(pSExpList);
             it != dynArrEnd(pSExpList);
             it = dynArrNext(pSExpList, it)) {
            result = evalDispatch(*it, env);
            if (!result) {
                // TODO:
                // evaluation failed, stop evaluation at this point
                // maybe giving more messages will be better
                printf("Failed to evaluate the expression at: %d / %d\n",
                       successCnt+1,
                       dynArrCount(pSExpList));
                break;
            } else {
                ++ successCnt;
            }
        }
        envFree(env);
        free(env);
        pointerManagerFinalize();
    } else {
        printf("Failed to parse the program.\n");
    }
    // releasing resources
    freeSExps(pSExpList);
    return NULL;
}
