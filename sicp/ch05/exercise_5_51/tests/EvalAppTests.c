#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalApp.h"
#include "../PointerManager.h"
#include "../InitEnv.h"

// simple application, identity function
START_TEST (test_EvalApp_simple1) {
    DynArr *pSExpList = parseSExps("((lambda (x) x) 1234)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    pointerManagerInit();
    Environment env = {0};
    envInit(&env);
    const SExp *actual = evApplication(*pExp, &env);
    ck_assert(isSExpEqual(actual, &expect));
    envFree(&env);
    pointerManagerFinalize();
    freeSExps(pSExpList);
} END_TEST

// simple application, const function this time
START_TEST (test_EvalApp_simple2) {
    DynArr *pSExpList = parseSExps("(((lambda (x) (lambda (y) x)) 1234) 4567)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    pointerManagerInit();
    Environment env = {0};
    envInit(&env);
    const SExp *actual = evApplication(*pExp, &env);
    ck_assert(isSExpEqual(actual, &expect));
    envFree(&env);
    pointerManagerFinalize();
    freeSExps(pSExpList);
} END_TEST

// apply primitive function
START_TEST (test_EvalApp_prim_app) {
    DynArr *pSExpList = parseSExps("(+ 123 456)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 579 } };
    SExp **pExp = dynArrBegin(pSExpList);
    pointerManagerInit();
    Environment *penv = mkInitEnv();
    const SExp *actual = evApplication(*pExp, penv);
    ck_assert(isSExpEqual(actual, &expect));
    envFree(penv);
    pointerManagerFinalize();
    freeSExps(pSExpList);
} END_TEST

Suite * evalAppSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalApp_simple1);
    tcase_add_test(tc_core, test_EvalApp_simple2);
    tcase_add_test(tc_core, test_EvalApp_prim_app);

    s = suite_create("EvalApp");
    suite_add_tcase(s, tc_core);
    return s;
}
