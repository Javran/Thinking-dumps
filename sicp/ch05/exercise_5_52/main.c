#include "Common.h"
#include "Util.h"
#include "SExp.h"
#include "Environment.h"

// source code:
/*

(+ 10 (* 20 30) (+ 5 6))

*/

// compiled code (in bytecode language)
/*

(assign proc (op lookup-variable-value) (const +) (reg env))
(save proc)
(save env)
(assign proc (op lookup-variable-value) (const +) (reg env))
(assign val (const 6))
(assign argl (op list) (reg val))
(assign val (const 5))
(assign argl (op cons) (reg val) (reg argl))
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

typedef struct {
    SExp *val;
    // TODO: not sure what type should I give to it
    Environment *env;
    void *cont;
    void *unev;
    void *proc;
    void *argl;
} Machine;

// all args are ignored
int main() {
    // TODO: function body
    return EXIT_FAILURE;
}
