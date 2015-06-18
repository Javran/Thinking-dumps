#include "Machine.h"

void evIf(Machine *m) {
    SExp *exp = m->exp.data.asSExp;

    SExp *expPred = sexpCadr(exp);
    SExp *expRemaining = sexpCddr(exp);
    SExp *expConseq = sexpCar(expRemaining);
    SExp *expAlter = sexpCadr(expRemaining);

}
