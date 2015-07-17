#ifndef _JAVEV_EVALCOND_H
#define _JAVEV_EVALCOND_H

#include "Evaluate.h"
#include "EvalSimple.h"

char isIf(const SExp *);
const SExp *evIf(const SExp *, Environment *);

SExpHandler ifHandler;

#endif
