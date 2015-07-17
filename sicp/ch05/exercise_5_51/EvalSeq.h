#ifndef JAVEV_EVALSEQ_H
#define JAVEV_EVALSEQ_H

#include "SExp.h"
#include "EvalSimple.h"

char isDefinition(const SExp *);
char isBegin(const SExp *);

const SExp *evSequence(const SExp *, Environment *);
const SExp *evBegin(const SExp *, Environment *);

SExpHandler beginHandler;

#endif
