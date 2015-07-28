#include "Primitives.h"
#include "SExp.h"

 const SExp * primPlus(const SExp *args) {
     DynArr *argsA = sexpProperListToDynArr(args);

     dynArrFree(argsA);
     free(argsA);
}
