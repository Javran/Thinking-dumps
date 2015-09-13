#ifndef JAVEV_MACHINE_H
#define JAVEV_MACHINE_H

#include "SExp.h"
#include "Environment.h"
#include "Stack.h"

typedef struct {
    SExp *val;
    // TODO: not sure what type should I give to them
    // maybe this is not important, as the code is generated,
    // an (void *) should be fine because the sanity check can
    // be done in the compiler
    // It won't hurt if we do double check here though.
    Environment *env;
    void *cont;
    void *unev;
    void *proc;
    void *argl;
    Stack *stk;
} Machine;

Machine *newMachine();
void freeMachine(Machine *);

#endif
