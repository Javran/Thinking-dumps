#include "Common.h"
#include "SExp.h"
#include "Environment.h"

// TODO: registers have to be typed (tagged),
// consider `(display a)`, without knowing the type of `a`,
// we have no knowledge of how to use it

// TODO: for now I have no idea about what could
// be a valid value for register to hold
typedef void *RegVal;

typedef struct {
    RegVal exp,env,val;
    RegVal cont,proc,argl,unev;
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
    m->val = m->exp;
}

SExpHandler selfEvaluatingHandler = {
    isSelfEvaluating,
    evalSelfEvaluating };

char isVariable(const SExp *p) {
    return sexpSymbol == p->tag;
}

void evalVariable(Machine *m) {
    SExp *exp = m->exp;
    const char *keyword = exp->fields.symbolName;
    Environment *env = m->env;
    FrameEntry *result = envLookup(env,keyword);

    // TODO: deal with lookup error
    assert( result );

    m->val = result->val;
}

SExpHandler variableHandler = {
    isVariable,
    evalVariable };

char isQuoted(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("quote", p->fields.pairContent.car);
}

void evalQuoted(Machine *m) {
    SExp *quotedExp = m->exp;
    SExp *eCdr = quotedExp->fields.pairContent.cdr;
    SExp *eCadr = eCdr->fields.pairContent.car;
    // TODO: validate
    m->val = eCadr;
}

SExpHandler quotedHandler = {
    isQuoted,
    evalQuoted };

// TODO:
// * assignment
// * definition
// * if
// * lambda
// * begin
// * application
