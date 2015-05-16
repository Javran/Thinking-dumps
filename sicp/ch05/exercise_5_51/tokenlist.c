#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

#include "evaluator.h"

Token **tokenList = NULL;
size_t tokenListCap = 0;
size_t tokenListCurMax = 0;

void tokenListInit() {
    assert( NULL == tokenList && 0 == tokenListCap );
    tokenListCap = SMALL_BUFFER_SIZE;
    tokenList = calloc(sizeof(Token *), tokenListCap);
}

void tokenListAdjust() {
    if (tokenListCurMax+1>=tokenListCap) {
        tokenListCap *= 2;
        tokenList = realloc(tokenList, sizeof(Token *) * tokenListCap);
        assert( tokenList );
        fprintf(stderr,"reallocation triggered\n");
    }
}

void tokenListInsert(Token *ptr) {
    tokenListAdjust();
    tokenList[tokenListCurMax] = ptr;
    ++tokenListCurMax;
}

Token* tokenListBegin() {
    return tokenList[0];
}

// assume elements have unique pointers
int tokenListIsLastElement(Token *ptr) {
    return ptr == tokenList[tokenListCurMax-1];
}

// should it be responsible freeing elements?
void tokenListFree() {
    if (tokenList != NULL) {
        free(tokenList);
        tokenList = NULL;
        tokenListCap = 0;
        tokenListCurMax = 0;
    }
}
