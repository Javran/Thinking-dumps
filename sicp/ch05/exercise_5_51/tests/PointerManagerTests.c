#include <stdlib.h>
#include <check.h>
#include "../PointerManager.h"

// registered pointers (handles) are freed properly
START_TEST (test_PointerManager_default_release) {
    pointerManagerInit();
    int i;
    for (i=0; i<10; ++i) {
        pointerManagerRegister(malloc(16*i));
    }
    pointerManagerFinalize();
} END_TEST

typedef struct {
    void *a;
    void *b;
} TestResource;

TestResource *newTestResource() {
    TestResource *retVal = calloc(1,sizeof(TestResource));
    retVal->a = malloc(16);
    retVal->b = malloc(32);
    return retVal;
}

void freeResource(TestResource *h) {
    free(h->a); free(h->b);
    free(h);
}

START_TEST (test_PointerManager_custom) {
    pointerManagerInit();
    pointerManagerRegisterCustom(newTestResource(),
                                 (PFreeCallback)freeResource);
    pointerManagerRegister(malloc(4000));
    pointerManagerRegister(malloc(6000));
    pointerManagerRegister(malloc(6000));
    pointerManagerRegister(malloc(2000));
    pointerManagerRegister(malloc(20));
    pointerManagerFinalize();
} END_TEST

Suite * pointerManagerSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_PointerManager_default_release);
    tcase_add_test(tc_core, test_PointerManager_custom);

    s = suite_create("PointerManager");
    suite_add_tcase(s, tc_core);
    return s;
}
