#ifndef _JAVEV_EVALSETDEFINE_H_
#define _JAVEV_EVALSETDEFINE_H_

#include "Machine.h"
#include "EvalSimple.h"

char isAssignment(const SExp *);
void evAssignment(const SExp *, Machine *);
char isDefinition(const SExp *);
void evDefinition(const SExp *, Machine *);

SExpHandler assignmentHandler;
SExpHandler definitionHandler;

#endif
