#include <stdlib.h>
#include <check.h>

extern Suite *dynArrSuite(void);
extern Suite *frameSuite(void);
extern Suite *environmentSuite(void);
extern Suite *evalSimpleSuite(void);
extern Suite *evalCondSuite(void);

int main(void) {
    Suite *sDynArr = dynArrSuite();
    Suite *sFrame = frameSuite();
    Suite *sEnvironment = environmentSuite();
    Suite *sEvalSimple = evalSimpleSuite();
    Suite *sEvalCond = evalCondSuite();

    SRunner *sr = srunner_create(sDynArr);
    srunner_add_suite(sr,sFrame);
    srunner_add_suite(sr,sEnvironment);
    srunner_add_suite(sr,sEvalSimple);
    srunner_add_suite(sr,sEvalCond);
    srunner_run_all(sr, CK_NORMAL);

    int number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);
    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}
