#ifndef JAVEV_MANAGEDSEXP_H
#define JAVEV_MANAGEDSEXP_H

#include "PointerManager.h"
#include "SExp.h"

SExp *managedSymbol(const char *);
SExp *managedString(const char *);
SExp *managedInteger(long);
SExp *managedBool(char);
SExp *managedNil();
SExp *managedPair(SExp *, SExp *);
SExp *managedFuncObject(void *);

#endif
