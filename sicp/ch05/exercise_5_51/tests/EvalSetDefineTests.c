#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalSetDefine.h"

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

Suite * evalSetDefineSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalSetDefine_set);

    s = suite_create("EvalSetDefine");
    suite_add_tcase(s, tc_core);
    return s;
}
