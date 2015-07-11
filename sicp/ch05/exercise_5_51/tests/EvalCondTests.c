#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalCond.h"

// if-expression, true-branch
START_TEST (test_EvalCond_true) {
    DynArr *pSExpList = parseSExps("(if #t 1234 5678)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evIf(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

// if-expression, false-branch
START_TEST (test_EvalCond_false) {
    DynArr *pSExpList = parseSExps("(if #f 1234 5678)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 5678} };
    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evIf(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

// if-expression, non-false-branch
// non-false is anything not equal to "#f", which should be considered
// true
START_TEST (test_EvalCond_non_false) {
    DynArr *pSExpList = parseSExps("(if '(a b c) 1234 5678)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evIf(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

Suite * evalCondSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalCond_true);
    tcase_add_test(tc_core, test_EvalCond_false);
    tcase_add_test(tc_core, test_EvalCond_non_false);

    s = suite_create("EvalCond");
    suite_add_tcase(s, tc_core);
    return s;
}
