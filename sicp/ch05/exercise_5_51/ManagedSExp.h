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
// same as newFuncObject, here we choose (void *) to break
// circular dependency issue, but it must be a pointer to
// a function object
const SExp *managedFuncObject(void *);

#endif
