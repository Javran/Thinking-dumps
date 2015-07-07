#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"

// DynArr should be initialized and freed successfully.
START_TEST (test_EvalSimple_int) {
    DynArr *pSExpList = parseSExps("1234", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);
    SExp expect = { sexpInteger, { .integerContent  = 1234} };
    SExp **pActual = dynArrBegin(pSExpList);

    ck_assert(isSExpEqual(&expect,*pActual));
    freeSExps(pSExpList);
} END_TEST

Suite * evalSimpleSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalSimple_int);

    s = suite_create("EvalSimple");
    suite_add_tcase(s, tc_core);
    return s;
}
