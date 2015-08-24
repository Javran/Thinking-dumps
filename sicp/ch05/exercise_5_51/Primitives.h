#ifndef JAVEV_PRIMITIVES_H
#define JAVEV_PRIMITIVES_H

#include "FunctionObject.h"

// Function primitives for the initial environment

SExp primPlusSExp;
SExp primMinusSExp;
SExp primMultSExp;

SExp primConsSExp;
SExp primCarSExp;
SExp primCdrSExp;
SExp primListSExp;

// "Q" for "?"
// "EQ" for "="
SExp primSymbolQSExp;
SExp primStringQSExp;
SExp primIntegerQSExp;
SExp primBooleanQSExp;
SExp primNullQSExp;
SExp primPairQSExp;

SExp primEQSExp;
SExp primEqQSExp;

SExp primNotSExp;
SExp primDisplaySExp;

#endif
