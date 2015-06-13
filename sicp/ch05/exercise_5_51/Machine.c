#include "Common.h"
#include "SExp.h"
#include "Environment.h"
#include "Register.h"

// TODO: new plan: implement operations as handlers

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

char isDefinition(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("define", p->fields.pairContent.car);
}

void evalSequence(Machine *m, const SExp *exps) {
    // TODO
}

void evDefinition(Machine *m) {
    SExp *exp = m->exp.data.asSExp;

    // TODO: function definition
    SExp *expVar = exp->fields.pairContent.cdr->fields.pairContent.car;
    SExp *expBody = exp->fields.pairContent.cdr->fields.pairContent.cdr;

    // TODO: need eval!
}

char isLambda(const SExp *p) {
    return sexpPair == p->tag
        && isSymbol("lambda", p->fields.pairContent.car);
}

typedef struct {
    // TODO: might change to some
    // other type in future.
    SExp* parameters;
    SExp* body;
    Environment* env;
} LambdaObject;

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

// TODO:
// * definition
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
