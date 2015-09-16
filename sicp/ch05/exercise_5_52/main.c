#include "Common.h"
#include "Util.h"
#include "Machine.h"
#include "ManagedSExp.h"

// source code:
/*

(+ 10 (* 20 30) (+ 5 6))

*/

// compiled code (in bytecode language)

void *lookupVariableValue(Environment *env, const char *keyword) {
    FrameEntry *result = envLookup(env,keyword);
    assert (result && "env lookup failed");
    return (void *)result->val;
}

// all args are ignored
int main() {
    Machine *m = newMachine();
    // TODO: function body

    // TODO: question: what to do with (const <???>)?
    // I guess the proper way would be treating them as SExps
    // or depending on how const are used, we deal with them differently

    // (assign proc (op lookup-variable-value) (const +) (reg env))
    m->proc = lookupVariableValue(m->env, "+");
    // (save proc)
    stackPush(m->stk, m->proc);
    // (save env)
    stackPush(m->stk, m->env);
    // (assign proc (op lookup-variable-value) (const +) (reg env))
    m->proc = lookupVariableValue(m->env, "+");

    // (assign val (const 6))
    m->val = (void *)managedInteger(6);
    // (assign argl (op list) (reg val))
    m->argl = (void *)managedPair(m->val, managedNil());
    // (assign val (const 5))
    m->val = (void *)managedInteger(5);
    // (assign argl (op cons) (reg val) (reg argl))
    m->argl = (void *)managedPair(m->val, m->argl);

/*

(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch6))
compiled-branch5
(assign continue (label after-call4))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch6
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call4
(assign argl (op list) (reg val))
(restore env)
(save argl)
(assign proc (op lookup-variable-value) (const *) (reg env))
(assign val (const 30))
(assign argl (op list) (reg val))
(assign val (const 20))
(assign argl (op cons) (reg val) (reg argl))
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch3))
compiled-branch2
(assign continue (label after-call1))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch3
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call1
(restore argl)
(assign argl (op cons) (reg val) (reg argl))
(assign val (const 10))
(assign argl (op cons) (reg val) (reg argl))
(restore proc)
(test (op primitive-procedure?) (reg proc))
(branch (label primitive-branch9))
compiled-branch8
(assign continue (label after-call7))
(assign val (op compiled-procedure-entry) (reg proc))
(goto (reg val))
primitive-branch9
(assign val (op apply-primitive-procedure) (reg proc) (reg argl))
after-call7

*/

    freeMachine(m);
    return EXIT_FAILURE;
}
