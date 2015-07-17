#ifndef JAVEV_EVALSIMPLE_H
#define JAVEV_EVALSIMPLE_H

#include "Evaluate.h"

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

#endif
