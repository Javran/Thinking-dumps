#include "Machine.h"

Machine *newMachine() {
    Machine *m = calloc(1,sizeof(Machine));
    stackInit(m->stk,sizeof(void *));
    return m;
}

void freeMachine(Machine *m) {
    stackFree(m->stk);
    free(m);
}
