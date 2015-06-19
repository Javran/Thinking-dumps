#include "SExp.h"
#include "Machine.h"
#include "EvalSimple.h"

#ifndef _JAVEV_EVALSEQ_H_
#define _JAVEV_EVALSEQ_H_

char isDefinition(const SExp *);
char isBegin(const SExp *);

void evSequence(const SExp *, Machine *);
void evBegin(const SExp*, Machine *);

SExpHandler beginHandler;

void evDefinition(Machine *);

#endif
