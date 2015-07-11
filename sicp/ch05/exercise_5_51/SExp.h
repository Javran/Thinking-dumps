#ifndef _JAVEV_SEXP_H_
#define _JAVEV_SEXP_H_

#include "Common.h"
#include "Environment.h"

// NOTE: one important thing to know about scheme is that
// the only "false" value is "#f", and any other thing is considered "true"
// so (if ? ? ?) can work even if the predicate part does not evaluate to a "sexpBool".
typedef enum {
    sexpSymbol,
    sexpString,
    sexpInteger,
    sexpBool,
    sexpNil,
    sexpPair,
    // this is a workaround for making lambda object pointers
    // a valid member of this type.
    // the lambda objects should be used internally only,
    // and equality tests involving lambda objects are invalid.
    sexpLamObj,
} SExpTag;

// for breaking circular definition,
// make compiler aware of it.
struct SExp;

typedef struct {
    struct SExp *car;
    struct SExp *cdr;
} PairContent;

typedef struct {
    // TODO: might change to some
    // other type in future.
    struct SExp* parameters;
    struct SExp* body;
    Environment* env;
} LambdaObject;

typedef union {
    char *symbolName;
    char *stringContent;
    long integerContent;
    char truthValue;
    PairContent pairContent;
    LambdaObject *pLamObj;
} SExpFields;

typedef struct SExp {
    SExpTag tag;
    SExpFields fields;
} SExp;

SExp *newSymbol(const char *);
SExp *newString(const char *);
SExp *newInteger(long);
SExp *newBool(char);
SExp *newNil();
SExp *newPair(SExp *, SExp *);
SExp *newLambdaObject(LambdaObject *);
void freeSExp(SExp *);
void printSExp(FILE *, SExp *);

char isSExpEqual(const SExp *, const SExp *);

static inline SExp *sexpCar(const SExp *e) { return e->fields.pairContent.car; }
static inline SExp *sexpCdr(const SExp *e) { return e->fields.pairContent.cdr; }
static inline SExp *sexpCadr(const SExp *e) { return sexpCar(sexpCdr(e)); }
static inline SExp *sexpCddr(const SExp *e) { return sexpCdr(sexpCdr(e)); }

static inline SExp *firstExp(const SExp *e) { return sexpCar(e); }
static inline SExp *restExps(const SExp *e) { return sexpCdr(e); }

// INVARIANT: "e" must be a non-empty proper list
static inline char isLastExp(const SExp *e) {
    return sexpNil == sexpCdr(e)->tag;
}

#endif
