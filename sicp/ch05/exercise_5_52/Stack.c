#include "Stack.h"

void stackInit(Stack *p, size_t sz) {
    dynArrInit(p, sz);
}

void stackPush(Stack *p, void * elem) {
    void ** cur = dynArrNew(p);
    *cur = elem;
}

void stackPop(Stack *p) {
    assert(p->elemMax >= 1 && "popping while stack is empty");
    p->elemMax -= 1;
}

void *stackTop(Stack *p) {
    return dynArrNth(p,p->elemMax - 1);
}

void stackFree(Stack *p) {
    dynArrFree(p);
}
