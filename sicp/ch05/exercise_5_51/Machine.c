#include "Common.h"
#include "SExp.h"
#include "Environment.h"
#include "Machine.h"

void evalDispatch(Machine *);

char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", sexpCar(p));
}

// correponding to ev-sequence
// requires unev to store the sequence of expressions
void evSequence(Machine *m) {
    // TODO: the list is assume to be non-empty
    while (! isLastExp(m->unev.data.asSExp) ) {
        SExp *firstExp = sexpCar( m->unev.data.asSExp );
        m->exp.tag = regSExp;
        m->exp.data.asSExp = firstExp;
        evalDispatch(m);
        m->unev.tag = regSExp;
        m->unev.data.asSExp = sexpCdr( m->unev.data.asSExp );
    }

    // otherwise this is the last expression, transfer control
    evalDispatch(m);
}

char isBegin(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("begin", sexpCar(p));
}

void evBegin(Machine *m) {
    SExp *beginActions = sexpCdr( m->exp.data.asSExp );
    m->unev.tag = regSExp;
    m->unev.data.asSExp = beginActions;
    evSequence(m);
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
