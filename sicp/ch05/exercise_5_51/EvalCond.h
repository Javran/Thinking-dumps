#ifndef _JAVEV_EVALCOND_H
#define _JAVEV_EVALCOND_H

#include "Machine.h"

char isIf(const SExp *);
void evIf(const SExp *, Machine *);

SExpHandler ifHandler;

#endif
