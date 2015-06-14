#include "Common.h"
#include "SExp.h"
#include "Environment.h"
#include "Machine.h"


char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", p->fields.pairContent.car);
}

// correponding to ev-sequence
// requires unev to store the sequence of expressions
void evSequence(Machine *m) {
    SExp *firstExp = m->unev.data.asSExp->fields.pairContent.car;
    // save?
    // we need a stack.
    // TODO: wrap dynarr to implement stack?
}

void evDefinition(Machine *m) {
    SExp *exp = m->exp.data.asSExp;

    // TODO: function definition
    SExp *expVar = exp->fields.pairContent.cdr->fields.pairContent.car;
    SExp *expBody = exp->fields.pairContent.cdr->fields.pairContent.cdr;

    // TODO: need eval!
}

// TODO:
// * definition
// * assignment
// * definition
// * if
// * begin
// * application

// TODO: it might be possible to make some of the arguments
// explicit. Although having access to the machine object is enough,
// I still think we can benefit from this.
void evalDispatch(Machine *m) {
    SExp *exp = m->exp.data.asSExp;

    // INVARIANT: every branch should end with a return
}
