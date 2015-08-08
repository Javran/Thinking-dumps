#include <stdlib.h>
#include <check.h>
#include "../Environment.h"

START_TEST (test_Environment_basic) {
    Environment envRoot = {0};
    Environment envLvl1 = {0};
    Environment envLvl2 = {0};

    envInit(&envRoot);

    envInit(&envLvl1);
    envSetParent(&envLvl1,&envRoot);

    envInit(&envLvl2);
    envSetParent(&envLvl2,&envLvl1);

    // as we know the implementation,
    // the conversion is safe because
    // we are only interested in the pointer value
    // but are not interested in the value the pointer points to
    char arrTest[] = {1,2,3,4,5,6,7,8};
    SExp *arr = (void *)arrTest;
    // Root: a -> 1, b -> 2
    // Lvl1: c -> 3, d -> 4
    // Lvl2: e -> 5, f -> 6
    envInsert(&envRoot,"a",&arr[0]);
    envInsert(&envRoot,"b",&arr[1]);

    envInsert(&envLvl1,"c",&arr[2]);
    envInsert(&envLvl1,"d",&arr[3]);

    envInsert(&envLvl2,"e",&arr[4]);
    envInsert(&envLvl2,"f",&arr[5]);

    // test looking up
    {
        FrameEntry *resultA = envLookup(&envLvl2,"a");
        FrameEntry *resultB = envLookup(&envLvl2,"b");
        FrameEntry *resultC = envLookup(&envLvl2,"c");
        FrameEntry *resultD = envLookup(&envLvl2,"d");
        FrameEntry *resultE = envLookup(&envLvl2,"e");
        FrameEntry *resultF = envLookup(&envLvl2,"f");
        FrameEntry *resultG = envLookup(&envLvl2,"g");

        ck_assert_ptr_ne(resultA, NULL);
        ck_assert_ptr_eq(resultA->val,&arr[0]);

        ck_assert_ptr_ne(resultB, NULL);
        ck_assert_ptr_eq(resultB->val,&arr[1]);

        ck_assert_ptr_ne(resultC, NULL);
        ck_assert_ptr_eq(resultC->val,&arr[2]);

        ck_assert_ptr_ne(resultD, NULL);
        ck_assert_ptr_eq(resultD->val,&arr[3]);

        ck_assert_ptr_ne(resultE, NULL);
        ck_assert_ptr_eq(resultE->val,&arr[4]);

        ck_assert_ptr_ne(resultF, NULL);
        ck_assert_ptr_eq(resultF->val,&arr[5]);

        // "g" is missing
        ck_assert_ptr_eq(resultG, NULL);
    }
    // "g" should be inserted into the current frame
    // g -> 7
    envInsert(&envLvl2,"g",&arr[6]);
    {
        FrameEntry *resultGR = envLookup(&envRoot,"g");
        FrameEntry *resultG1 = envLookup(&envLvl1,"g");
        FrameEntry *resultG2 = envLookup(&envLvl2,"g");

        ck_assert_ptr_eq(resultGR, NULL);
        ck_assert_ptr_eq(resultG1, NULL);
        ck_assert_ptr_ne(resultG2, NULL);
        ck_assert_ptr_eq(resultG2->val,&arr[6]);
    }

    // "a" shadowing test
    // a -> 8
    envInsert(&envLvl2,"a",&arr[7]);
    {
        FrameEntry *resultAR = envLookup(&envRoot,"a");
        FrameEntry *resultA1 = envLookup(&envLvl1,"a");
        FrameEntry *resultA2 = envLookup(&envLvl2,"a");

        ck_assert_ptr_ne(resultAR, NULL);
        ck_assert_ptr_eq(resultAR->val,&arr[0]);

        ck_assert_ptr_ne(resultA1, NULL);
        ck_assert_ptr_eq(resultA1->val,&arr[0]);

        ck_assert_ptr_ne(resultA2, NULL);
        ck_assert_ptr_eq(resultA2->val,&arr[7]);
    }

    envFree(&envLvl2);
    envFree(&envLvl1);
    envFree(&envRoot);
} END_TEST

Suite * environmentSuite(void) {
    Suite *s;
    TCase *tc_core;
    tc_core = tcase_create("Core");
    tcase_add_test(tc_core, test_Environment_basic);
    s = suite_create("Environment");
    suite_add_tcase(s, tc_core);
    return s;
}
