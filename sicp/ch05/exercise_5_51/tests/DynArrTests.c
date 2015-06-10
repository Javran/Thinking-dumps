#include <stdlib.h>
#include <check.h>
#include "../DynArr.h"

// DynArr should be initialized and freed successfully.
START_TEST (test_DynArr_init_free) {
    DynArr da = {0};
    dynArrInit(&da, sizeof(int));
    dynArrFree(&da);
} END_TEST

int *testFold(int *state, int *next) {
    *state = *state + *next;
    return state;
}

// DynArr foldl test
START_TEST (test_DynArr_foldl) {
   DynArr testA = {0};
    dynArrInit(&testA, sizeof(int));
    int i;
    int state = 0;
    for (i = 0; i <= 100; ++i) {
        int *p = dynArrNew(&testA);
        *p = i;
    }
    // fold left
    dynArrFoldLeft(&testA,(DynArrFoldLeftAccumulator)testFold,&state);
    dynArrFree(&testA);

    ck_assert_int_eq(state, 5050);
} END_TEST

Suite * dynArrSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_DynArr_init_free);
    tcase_add_test(tc_core, test_DynArr_foldl);

    s = suite_create("DynArr");
    suite_add_tcase(s, tc_core);
    return s;
}
