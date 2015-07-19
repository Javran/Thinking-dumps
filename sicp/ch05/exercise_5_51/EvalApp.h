#ifndef JAVEV_EVALAPP_H
#define JAVEV_EVALAPP_H

#include "Evaluate.h"

char isApplication(const SExp *);
const SExp *evApplication(const SExp *, Environment *);

SExpHandler applicationHandler;

#endif
