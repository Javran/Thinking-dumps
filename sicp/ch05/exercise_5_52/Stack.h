#ifndef JAVEV_STACK_H
#define JAVEV_STACK_H

#include "DynArr.h"

typedef DynArr Stack;

void stackInit(Stack *, size_t);
void stackPush(Stack *, void *);
void stackPop(Stack *);
void stackFree(Stack *);

#endif
