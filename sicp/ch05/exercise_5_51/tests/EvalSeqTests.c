#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalSeq.h"

// with one expression
START_TEST (test_EvalSeq_one) {
    DynArr *pSExpList = parseSExps("(begin 1234)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent  = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    // note that NULL is now a valid environment with no bindings
    const SExp *result = evBegin(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

Suite * evalSeqSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalSeq_one);

    s = suite_create("EvalSeq");
    suite_add_tcase(s, tc_core);
    return s;
}
