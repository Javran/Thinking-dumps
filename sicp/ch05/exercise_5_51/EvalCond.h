#include "Machine.h"

#ifndef _JAVEV_EVALCOND_H
#define _JAVEV_EVALCOND_H

char isIf(const SExp *);
void evIf(const SExp *, Machine *);

SExpHandler ifHandler;

#endif
