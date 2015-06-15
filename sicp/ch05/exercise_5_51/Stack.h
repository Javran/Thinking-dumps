#include "DynArr.h"

#ifndef _JAVEV_STACK_H_
#define _JAVEV_STACK_H_

typedef struct {
    DynArr content;
    size_t size;
} Stack;

void stackInit(Stack *, size_t);
void *stackPush(Stack *);
void *stackTop(Stack *);
void stackPop(Stack *);
char stackIsEmpty(Stack *);
void stackFree(Stack *);

#endif
