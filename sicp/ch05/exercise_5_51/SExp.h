#include "Common.h"

#ifndef _JAVEV_SEXP_H_
#define _JAVEV_SEXP_H_

typedef enum {
    sexpSymbol,
    sexpString,
    sexpInteger,
    sexpBool,
    sexpNil,
    sexpPair,
} SExpTag;

// for breaking circular definition,
// make compiler aware of it.
struct SExp;

typedef struct {
    struct SExp *car;
    struct SExp *cdr;
} PairContent;

typedef union {
    char *symbolName;
    char *stringContent;
    long integerContent;
    char truthValue;
    PairContent pairContent;
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
