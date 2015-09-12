#include "DynArr.h"

typedef DynArr Stack;

void stackInit(Stack *p, size_t sz) {
    dynArrInit(p, sz);
}

void stackPush(Stack *p, void * elem) {
    void ** cur = dynArrNew(p);
    *cur = elem;
}

void stackFree(Stack *p) {
    dynArrFree(p);
}
