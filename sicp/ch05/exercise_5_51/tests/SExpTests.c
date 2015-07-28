#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../SExp.h"

START_TEST (test_SExp_properlist_to_dynarr) {
    // parse the list for construction
    DynArr *pSExpList = parseSExps("(1 2 3 4)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);
    SExp **pExp = dynArrBegin(pSExpList);

    DynArr *da = sexpProperListToDynArr(*pExp);

    // test de-allocation
    dynArrFree(da);
    free(da);

    freeSExps(pSExpList);
} END_TEST

Suite * sexpSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_SExp_properlist_to_dynarr);

    s = suite_create("SExp");
    suite_add_tcase(s, tc_core);
    return s;
}
