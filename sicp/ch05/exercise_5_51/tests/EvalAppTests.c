#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalApp.h"
#include "../PointerManager.h"

// simple application, identity function
START_TEST (test_EvalApp_simple) {
    DynArr *pSExpList = parseSExps("((lambda (x) x) 1234)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp **pExp = dynArrBegin(pSExpList);
    pointerManagerInit();
    const SExp *result = evApplication(*pExp, NULL);

    pointerManagerFinalize();
    freeSExps(pSExpList);
} END_TEST

Suite * evalAppSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalApp_simple);

    s = suite_create("EvalApp");
    suite_add_tcase(s, tc_core);
    return s;
}
