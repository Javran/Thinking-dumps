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

Suite * pointerManagerSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_PointerManager_default_release);

    s = suite_create("PointerManager");
    suite_add_tcase(s, tc_core);
    return s;
}
