#include <stdlib.h>
#include <check.h>

extern Suite *dynArrSuite(void);
extern Suite *frameSuite(void);
extern Suite *environmentSuite(void);

int main(void) {
    Suite *sDynArr = dynArrSuite();
    Suite *sFrame = frameSuite();
    Suite *sEnvironment = environmentSuite();

    SRunner *sr = srunner_create(sDynArr);
    srunner_add_suite(sr,sFrame);
    srunner_add_suite(sr,sEnvironment);
    srunner_run_all(sr, CK_NORMAL);

    int number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
