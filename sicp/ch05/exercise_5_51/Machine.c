#include "Common.h"
#include "SExp.h"
#include "Environment.h"
#include "Machine.h"

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

// TODO: it might be possible to make some of the arguments
// explicit. Although having access to the machine object is enough,
// I still think we can benefit from this.
void evalDispatch(const SExp *exp, Machine *m) {
    // INVARIANT: every branch should end with a return
    size_t i;
    for (i=0; i<evalHandlerCount; ++i) {
        SExpHandler *h = evalHandlers[i];
        if (h->pred(exp)) {
            h->eval(exp,m);
            return;
        }
    }

    // TODO: not handled expressions
}
