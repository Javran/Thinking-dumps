#ifndef JAVEV_EVALUATE_H
#define JAVEV_EVALUATE_H

#include "Common.h"
#include "Parser.h"

#include "SExp.h"
#include "Environment.h"

// S-expresion related.
typedef char (*SExpPredicate)(const SExp *);
typedef const SExp *(*SExpEval)(const SExp *, Environment *);

// I started working on this exercise by implementing
// the machine first, then I realized the machine and many other components
// are not actually necessary. Take registers as an example,
// in assembly codes we need to manually maintain registers, push / pop values
// into a limited number of registers, but for programming languages like C,
// we don't have to do this ourselves, because we can have infinite number
// of variables and let the compiler deal with register-handling.
// it's not cheating but taking advantage of the things we have
// in a programming language. For C programming language, it can be assumed
// to run on a machine that can have infinite number of register and stack,
// procedure are called properly with proper arguments loaded -- all these
// has already be handled by the implementation of the language so we don't
// have to worry about them.

// After we taking advantages of C programming language, this is basically
// becoming a straightforward lisp interpreter.

// an s-expression handler
// consists of a predicate and an evaluator
typedef struct {
    // invariant: the pointer can never be NULL
    SExpPredicate pred;
    // invariant: the s-exp must meet the requirement
    // that "pred" has specified
    SExpEval eval;
} SExpHandler;

const SExp *evalDispatch(const SExp *, Environment *);

SExp *evalProgramText(const char *, FILE *);

#endif
