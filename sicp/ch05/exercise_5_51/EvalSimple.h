#include "Machine.h"

#ifndef _JAVEV_EVALSIMPLE_H_
#define _JAVEV_EVALSIMPLE_H_

char isSymbol(const char *, const SExp *);
char isSelfEvaluating(const SExp *);
char isVariable(const SExp *);
char isQuoted(const SExp *);
char isLambda(const SExp *);

const SExp *evSelfEval(const SExp*, Environment *);
const SExp *evVariable(const SExp*, Environment *);
const SExp *evQuoted(const SExp*, Environment *);
const SExp *evLambda(const SExp*, Environment *);

SExpHandler selfEvaluatingHandler;
SExpHandler variableHandler;
SExpHandler quotedHandler;
SExpHandler lambdaHandler;

typedef struct {
    // TODO: might change to some
    // other type in future.
    SExp* parameters;
    SExp* body;
    Environment* env;
} LambdaObject;

#endif
