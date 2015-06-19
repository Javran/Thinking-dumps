#include "Common.h"
#include "SExp.h"
#include "Environment.h"
#include "Machine.h"

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
void evalDispatch(const SExp *exp, Machine *m) {
    // INVARIANT: every branch should end with a return
}
