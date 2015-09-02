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
#include "EvalLet.h"

#include "InitEnv.h"

// TODO: handle let-expression

SExpHandler *evalHandlers[] = {
    &selfEvaluatingHandler,
    &variableHandler,
    &quotedHandler,
    &assignmentHandler,
    &definitionHandler,
    &ifHandler,
    &lambdaHandler,
    &letHandler,
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

// note that by the time the program returns, the PointerManager is already
// released, making it not possible to return a "managed" result.
// so instead we just return whether the execution was successful.
char evalProgramText(const char *programText, FILE *errF) {
    DynArr *pSExpList = parseSExps(programText, errF);
    // it is guaranteed that parseStateCurrent always produces
    // a valid pointer. no check is necessary.
    char parseFailed = !pSExpList;
    char successFlag = 1;

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
                successFlag = 0;
                printf("\nFailed to evaluate the expression at: %d / %d\n",
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
    return successFlag;
}
