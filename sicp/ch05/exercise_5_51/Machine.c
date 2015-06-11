#include "Common.h"
#include "SExp.h"
#include "Environment.h"
#include "Register.h"

// TODO: new plan: implement operations as handlers

// TODO: rename procedures with its corresponding label names

// TODO: registers have to be typed (tagged),
// consider `(display a)`, without knowing the type of `a`,
// we have no knowledge of how to use it

typedef struct {
    // INVARIANT: exp is always holding an expression
    Register exp,env,val;
    Register cont,proc,argl,unev;
} Machine;

// S-expresion related.
typedef char (*SExpPredicate)(const SExp *);
typedef void (*SExpEval)(Machine *);

// an s-expression handler
// consists of a predicate and an evaluator
typedef struct {
    // invariant: the pointer can never be NULL
    SExpPredicate pred;
    // invariant: the s-exp must meet the requirement
    // that "pred" has specified
    SExpEval eval;
} SExpHandler;

// for simplicity, case-insensitive comparison
// is NOT implemented
char isSymbol(const char *symbol, const SExp *p) {
    return sexpSymbol == p->tag
        && 0 == strcmp(symbol, p->fields.symbolName);
}

char isSelfEvaluating(const SExp *p) {
    switch (p->tag) {
    case sexpInteger:
    case sexpString:
    case sexpBool:
        return 1;
    default:
        return 0;
    }
}

void evalSelfEvaluating(Machine *m) {
    // shallow copy
    m->val = m->exp;
}

SExpHandler selfEvaluatingHandler = {
    isSelfEvaluating,
    evalSelfEvaluating };

char isVariable(const SExp *p) {
    return sexpSymbol == p->tag;
}

void evalVariable(Machine *m) {
    SExp *exp = m->exp.data.asSExp;
    const char *keyword = exp->fields.symbolName;
    Environment *env = m->env.data.asEnv;
    FrameEntry *result = envLookup(env,keyword);

    // TODO: deal with lookup error
    assert( result );

    Register *r = result->val;
    m->val = *r;
}

SExpHandler variableHandler = {
    isVariable,
    evalVariable };

char isQuoted(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("quote", p->fields.pairContent.car);
}

void evalQuoted(Machine *m) {
    SExp *quotedExp = m->exp.data.asSExp;
    SExp *eCdr = quotedExp->fields.pairContent.cdr;
    SExp *eCadr = eCdr->fields.pairContent.car;
    // TODO: validate
    m->val.tag = regSExp;
    m->val.data.asSExp = eCadr;
}

SExpHandler quotedHandler = {
    isQuoted,
    evalQuoted };

char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", p->fields.pairContent.car);
}

void evalSequence(Machine *m, const SExp *exps) {
    
}

void evalDefinition(Machine *m) {
    SExp *exp = m->exp.data.asSExp;

    // TODO: function definition
    SExp *expVar = exp->fields.pairContent.cdr->fields.pairContent.car;

    SExp *expBody = exp->fields.pairContent.cdr->fields.pairContent.cdr;

    // TODO: need eval!
}

// TODO:
// * assignment
// * definition
// * if
// * lambda
// * begin
// * application

// TODO: it might be possible to make some of the arguments
// explicit. Although having access to the machine object is enough,
// I still think we can benefit from this.
void evalDispatch(Machine *m) {
    SExp *exp = m->exp.data.asSExp;

    // INVARIANT: every branch should end with a return

}
