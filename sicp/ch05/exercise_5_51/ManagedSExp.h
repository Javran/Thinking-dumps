#ifndef JAVEV_MANAGEDSEXP_H
#define JAVEV_MANAGEDSEXP_H

#include "PointerManager.h"
#include "SExp.h"

const SExp *managedSymbol(const char *);
const SExp *managedString(const char *);
const SExp *managedInteger(long);
const SExp *managedBool(char);
const SExp *managedNil();
const SExp *managedPair(const SExp *, const SExp *);
const SExp *managedFuncObject(void *);

#endif
