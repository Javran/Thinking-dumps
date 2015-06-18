// an simple expression is one of the following:
// * self-evaluating expression
// * variable
// * quoted expression
// * lambda expression
// I guess they are "simple" in terms of
// the complexity of implementation.

#include "EvalSimple.h"

// TODO: registers have to be typed (tagged),
// consider `(display a)`, without knowing the type of `a`,
// we have no knowledge of how to use it

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

void evSelfEval(Machine *m) {
    // shallow copy
    m->val = m->exp;
}

SExpHandler selfEvaluatingHandler = {
    isSelfEvaluating,
    evSelfEval };


char isVariable(const SExp *p) {
    return sexpSymbol == p->tag;
}

void evVariable(Machine *m) {
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
    evVariable };

char isQuoted(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("quote", p->fields.pairContent.car);
}

void evQuoted(Machine *m) {
    SExp *quotedExp = m->exp.data.asSExp;
    SExp *eCdr = quotedExp->fields.pairContent.cdr;
    SExp *eCadr = eCdr->fields.pairContent.car;
    // TODO: validate
    m->val.tag = regSExp;
    m->val.data.asSExp = eCadr;
}

SExpHandler quotedHandler = {
    isQuoted,
    evQuoted };

char isLambda(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("lambda", p->fields.pairContent.car);
}

void evLambda(Machine *m) {
    // (lambda (x y z) x x z)
    // * lambda-parameters: (x y z) -- cadr gives the parameters
    // * lambda-body: (x x z)       -- cddr gives the body
    SExp *cdr = m->exp.data.asSExp->fields.pairContent.cdr;
    SExp *lamParam = cdr->fields.pairContent.car;
    SExp *lamBody = cdr->fields.pairContent.cdr;

    LambdaObject *lo = calloc(1,sizeof(LambdaObject));
    lo->parameters = lamParam;
    lo->body = lamBody;
    lo->env = m->env.data.asEnv;

    m->val.tag = regLamda;
    m->val.data.asLambda = lo;
}

SExpHandler lambdaHandler = {
    isLambda,
    evLambda };
