#include <stdlib.h>
#include <check.h>
#include "../DynArr.h"

// DynArr should be initialized and freed successfully.
START_TEST (test_DynArr_init_free)
{
    DynArr da = {0};
    dynArrInit(&da, sizeof(int));
    dynArrFree(&da);
}
END_TEST



Suite * dynArrSuite(void)
{
    Suite *s;
    TCase *tc_core;
    s = suite_create("DynArr");

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_DynArr_init_free);
//    tcase_add_test(tc_core, test_DynArr_foldl);

//    tcase_add_test(tc_core, test_Frame_basic);

//    tcase_add_test(tc_core, test_Environment_basic);

    suite_add_tcase(s, tc_core);
    return s;
}
