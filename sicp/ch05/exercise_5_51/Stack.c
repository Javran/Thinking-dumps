#include "Common.h"
#include "Stack.h"

void stackInit(Stack *stk, size_t elemSz) {
    assert( !stk->size );
    dynArrInit(&stk->content, elemSz);
}

char stackIsEmpty(Stack *stk) {
    return !stk->size;
}

void stackFree(Stack *stk) {
    dynArrFree(&stk->content);
}

void *stackTop(Stack *stk) {
    return dynArrNth(&stk->content, stk->size-1);
}

void stackPop(Stack *stk) {
    -- stk->size;
}

void *stackPush(Stack *stk) {
    ++ stk->size;
    if (stk->size > stk->content.elemSize) {
        dynArrNew(&stk->content);
    }
    return stackTop(stk);
}
// TODO: tests
