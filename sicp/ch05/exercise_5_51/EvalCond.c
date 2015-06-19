#include "Machine.h"

void evIf(const SExp *exp, Machine *m) {
    SExp *expPred = sexpCadr(exp);
    SExp *expRemaining = sexpCddr(exp);
    SExp *expConseq = sexpCar(expRemaining);
    SExp *expAlter = sexpCadr(expRemaining);
}
