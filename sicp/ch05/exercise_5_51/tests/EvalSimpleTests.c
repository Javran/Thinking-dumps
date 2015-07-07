#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalSimple.h"

// evaluating simple expressions
START_TEST (test_EvalSimple_int) {
    DynArr *pSExpList = parseSExps("1234", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent  = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    // TODO: environment?
    const SExp *result = evSelfEval(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST


START_TEST (test_EvalSimple_str) {
    DynArr *pSExpList = parseSExps("\"string!\"", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpString, { .stringContent = "string!"} };
    SExp **pExp = dynArrBegin(pSExpList);
    // TODO: environment?
    const SExp *result = evSelfEval(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

/*
START_TEST (test_EvalSimple_quote1) {
    DynArr *pSExpList = parseSExps("(quote quoted)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpSymbol, { .symbolName = "quoted" } };
    SExp **pActual = dynArrBegin(pSExpList);
    ck_assert(isSExpEqual(&expect,*pActual));

    freeSExps(pSExpList);
} END_TEST */


Suite * evalSimpleSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalSimple_int);
    tcase_add_test(tc_core, test_EvalSimple_str);
//    tcase_add_test(tc_core, test_EvalSimple_quote1);

    s = suite_create("EvalSimple");
    suite_add_tcase(s, tc_core);
    return s;
}
