#ifndef JAVEV_EVALLET_H
#define JAVEV_EVALLET_H

#include "Evaluate.h"

char isLetExpression(const SExp *);

const SExp *evLet(const SExp*, Environment *);

SExpHandler letHandler;

#endif
