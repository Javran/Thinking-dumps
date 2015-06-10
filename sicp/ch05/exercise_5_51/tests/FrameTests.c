#include <stdlib.h>
#include <check.h>
#include "../Frame.h"

// basic functionality of Frame
START_TEST (test_Frame_basic)
{
    int testArr[] = {0,1,2,3};
    Frame fr = {0};

    frameInit(&fr);

    frameInsert(&fr,"key_A",&testArr[0]);
    frameInsert(&fr,"key_B",&testArr[1]);
    frameInsert(&fr,"key_C",&testArr[2]);

    FrameEntry *resultA = frameLookup(&fr,"key_A");
    FrameEntry *resultB = frameLookup(&fr,"key_B");
    FrameEntry *resultC = frameLookup(&fr,"key_C");
    FrameEntry *resultD = frameLookup(&fr,"key_D");

    ck_assert_ptr_ne(resultA, NULL);
    ck_assert_ptr_eq(resultA->val,&testArr[0]);

    ck_assert_ptr_ne(resultB, NULL);
    ck_assert_ptr_eq(resultB->val,&testArr[1]);

    ck_assert_ptr_ne(resultC, NULL);
    ck_assert_ptr_eq(resultC->val,&testArr[2]);

    ck_assert_ptr_eq(resultD, NULL);

    frameFree(&fr);
}
END_TEST

Suite * frameSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_Frame_basic);

    s = suite_create("Frame");
    suite_add_tcase(s, tc_core);
    return s;
}
