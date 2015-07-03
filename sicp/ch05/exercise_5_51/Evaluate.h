#ifndef _JAVEV_EVALUATE_H_
#define _JAVEV_EVALUATE_H_

#include "Common.h"
#include "Parser.h"
#include "Machine.h"

SExp *evalProgramText(const char *, FILE *);

#endif
