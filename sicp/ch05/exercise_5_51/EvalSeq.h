#include "SExp.h"
#include "Machine.h"
#include "EvalSimple.h"

#ifndef _JAVEV_EVALSEQ_H_
#define _JAVEV_EVALSEQ_H_

char isDefinition(const SExp *);
char isBegin(const SExp *);

void evSequence(Machine *);
void evBegin(Machine *);

SExpHandler beginHandler;

void evDefinition(Machine *);

#endif
