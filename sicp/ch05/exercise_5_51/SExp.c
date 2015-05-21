#include "Common.h"

typedef struct SExp SExp;

typedef enum {
    sexpSymbol,
    sexpString,
    sexpInteger,
    sexpBool,
    sexpNil,
    sexpPair,
} SExpTag;

typedef struct {
    SExp *car;
    SExp *cdr;
} PairContent;

typedef union {
    char *symbolName;
    char *stringContent;
    long integerContent;
    char truthValue;
    PairContent pairContent;
} SExpFields;

struct SExp {
    SExpTag tag;
    SExpFields fields;
};
