#ifndef _JAVEV_EVALSEQ_H_
#define _JAVEV_EVALSEQ_H_

#include "SExp.h"
#include "Machine.h"
#include "EvalSimple.h"

char isDefinition(const SExp *);
char isBegin(const SExp *);

const SExp *evSequence(const SExp *, Environment *);
const SExp *evBegin(const SExp *, Environment *);

SExpHandler beginHandler;

#endif