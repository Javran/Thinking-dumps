#include <stdlib.h>
#include <check.h>
#include "../DynArr.h"
#include "../Frame.h"

// DynArr should be initialized and freed successfully.
START_TEST (test_DynArr_init_free)
{
    DynArr da = {0};
    dynArrInit(&da, sizeof(int));
    dynArrFree(&da);
}
END_TEST

int *testFold(int *state, int *next) {
    *state = *state + *next;
    return state;
}

// DynArr foldl test
START_TEST (test_DynArr_foldl)
{
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
}
END_TEST

// basic functionality of Frame
START_TEST (test_Frame_basic)
{
    int testArr[] = {0,1,2,3};
    Frame fr = {0};

    frameInit(&fr);

    frameInsert(&fr,"key_A",&testArr[0]);
    frameInsert(&fr,"key_B",&testArr[1]);
    frameInsert(&fr,"key_C",&testArr[2]);

    frameFree(&fr);

}
END_TEST

Suite * money_suite(void)
{
    Suite *s;
    TCase *tc_core;
    s = suite_create("Money");

    /* Core test case */
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_DynArr_init_free);
    tcase_add_test(tc_core, test_DynArr_foldl);

    tcase_add_test(tc_core, test_Frame_basic);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void) {
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = money_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
