#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalSetDefine.h"
#include "../PointerManager.h"
#include "../SExp.h"
#include "../FunctionObject.h"

// set! expression
START_TEST (test_EvalSetDefine_set) {
    DynArr *pSExpList = parseSExps("(set! var 4567)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp initVal = { sexpInteger, { .integerContent = 1234} };
    SExp expect = { sexpInteger, { .integerContent = 4567} };
    SExp **pExp = dynArrBegin(pSExpList);
    SExp *nil = newNil();

    Environment env = {0};
    envInit(&env);
    envInsert(&env, "var", &initVal);

    const SExp *result = evAssignment(*pExp, &env);
    ck_assert_ptr_eq(result, nil);
    ck_assert(isSExpEqual(nil,result));

    const FrameEntry *fe = envLookup(&env, "var");
    ck_assert_ptr_ne(fe, NULL);
    ck_assert_ptr_ne(fe->val, NULL);

    const SExp *actual = fe->val;
    ck_assert(isSExpEqual(actual, &expect));
    envFree(&env);
    freeSExps(pSExpList);
} END_TEST

// define a simple expression
START_TEST (test_EvalSetDefine_define_simple) {
    DynArr *pSExpList = parseSExps("(define var 1234)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    SExp *nil = newNil();

    Environment env = {0};
    envInit(&env);

    const SExp *result = evDefinition(*pExp, &env);

    ck_assert(isSExpEqual(nil,result));

    const FrameEntry *fe = envLookup(&env, "var");
    ck_assert_ptr_ne(fe, NULL);
    ck_assert_ptr_ne(fe->val, NULL);

    const SExp *actual = fe->val;
    ck_assert(isSExpEqual(actual, &expect));
    envFree(&env);
    freeSExps(pSExpList);
} END_TEST

// define a function
START_TEST (test_EvalSetDefine_define_func) {
    DynArr *pSExpList = parseSExps("(define (id-like x) 'a 'b x)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp **pExp = dynArrBegin(pSExpList);
    SExp *nil = newNil();

    Environment env = {0};
    envInit(&env);

    pointerManagerInit();
    const SExp *result = evDefinition(*pExp, &env);

    ck_assert(isSExpEqual(nil,result));

    const FrameEntry *fe = envLookup(&env, "id-like");

    ck_assert_ptr_ne(fe, NULL);
    ck_assert_ptr_ne(fe->val, NULL);

    // we should not rely on the result of equality test on FunctionObjects,
    // instead we want to know whether each component of the FunctionObject
    // is the same

    SExp *sexpL = fe->val;
    FuncObj *fo = sexpL->fields.pFuncObj;

    // a compound function object should now be bound as
    // a value
    ck_assert( funcCompound == fo->tag);

    // check compound object
    SExp *args = sexpCdr( sexpCadr(*pExp) );
    SExp *body = sexpCddr( *pExp );

    ck_assert( isSExpEqual(args, fo->fields.compObj.parameters) );
    ck_assert( isSExpEqual(body, fo->fields.compObj.body) );

    // as the address of "env" is not-relocated during evaluation,
    // we can also verify it
    ck_assert_ptr_eq( &env, fo->fields.compObj.env );

    pointerManagerFinalize();
    envFree(&env);
    freeSExps(pSExpList);
} END_TEST

Suite * evalSetDefineSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalSetDefine_set);
    tcase_add_test(tc_core, test_EvalSetDefine_define_simple);
    tcase_add_test(tc_core, test_EvalSetDefine_define_func);

    s = suite_create("EvalSetDefine");
    suite_add_tcase(s, tc_core);
    return s;
}
