#ifndef JAVEV_EVALSETDEFINE_H
#define JAVEV_EVALSETDEFINE_H

#include "EvalSimple.h"

char isAssignment(const SExp *);
const SExp *evAssignment(const SExp *, Environment *);
char isDefinition(const SExp *);
const SExp *evDefinition(const SExp *, Environment *);

SExpHandler assignmentHandler;
SExpHandler definitionHandler;

#endif
