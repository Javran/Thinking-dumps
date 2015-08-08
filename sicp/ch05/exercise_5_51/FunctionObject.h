#ifndef JAVEV_FUNCTIONOBJECT_H
#define JAVEV_FUNCTIONOBJECT_H

#include "SExp.h"
#include "Environment.h"

// every function and lambda expression
// is eventually represented in a FunctionObject,
// which can either be a primitive operation or
// an lambda expression (with closure)

typedef enum {
    funcPrim,
    funcCompound
} FuncObjTag;

typedef struct {
    const SExp *parameters;
    const SExp *body;
    Environment *env;
} FuncCompound;

// a primtive function handler consumes a S-expression
// of proper list
// TODO: consider converting it to list which might work better?
typedef const SExp * (*FuncPrimHandler)(const SExp *);

typedef union {
    FuncPrimHandler primHdlr;
    FuncCompound compObj;
} FuncFields;

typedef struct {
    FuncObjTag tag;
    FuncFields fields;
} FuncObj;

FuncObj *newCompoundFunc(const SExp *, const SExp *, Environment *);
const SExp *funcObjApp(const FuncObj *, const SExp *, Environment *);
void freeFuncObject(FuncObj *);

#endif
