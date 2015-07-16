#include <stdlib.h>
#include <check.h>
#include "../Evaluate.h"
#include "../EvalSimple.h"

// evaluating simple expressions
START_TEST (test_EvalSimple_int) {
    DynArr *pSExpList = parseSExps("1234", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent  = 1234} };
    SExp **pExp = dynArrBegin(pSExpList);
    // note that NULL is now a valid environment with no bindings
    const SExp *result = evSelfEval(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

// boolean value, true
START_TEST (test_EvalSimple_boot_true) {
    DynArr *pSExpList = parseSExps("#t", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpBool, { .truthValue = 1 } };
    SExp **pExp = dynArrBegin(pSExpList);
    // note that NULL is now a valid environment with no bindings
    const SExp *result = evSelfEval(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

// boolean value, false
START_TEST (test_EvalSimple_boot_false) {
    DynArr *pSExpList = parseSExps("#f", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpBool, { .truthValue = 0 } };
    SExp **pExp = dynArrBegin(pSExpList);
    // note that NULL is now a valid environment with no bindings
    const SExp *result = evSelfEval(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

// simple string
START_TEST (test_EvalSimple_str) {
    DynArr *pSExpList = parseSExps("\"string!\"", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpString, { .stringContent = "string!"} };
    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evSelfEval(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

// a quoted expression
START_TEST (test_EvalSimple_quote1) {
    DynArr *pSExpList = parseSExps("(quote quoted)", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpSymbol, { .symbolName = "quoted" } };
    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evQuoted(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
} END_TEST

// a quoted expression (with more complicated one inside)
START_TEST (test_EvalSimple_quote2) {
    DynArr *pSExpList = parseSExps("(quote (a b c))", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp *nil = newNil();
    SExp *sa = newSymbol("a");
    SExp *sb = newSymbol("b");
    SExp *sc = newSymbol("c");
    SExp *expect = newPair(sa,newPair(sb,newPair(sc,nil)));
    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evQuoted(*pExp, NULL);

    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(expect,result));
    freeSExps(pSExpList);
    freeSExp(expect);
} END_TEST

// simple variable test
START_TEST (test_EvalSimple_var1) {
    DynArr *pSExpList = parseSExps("var", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 12345 } };
    Environment env = {0};
    envInit(&env);
    envInsert(&env, "var", &expect);
    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evVariable(*pExp, &env);
    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&expect,result));
    freeSExps(pSExpList);
    envFree(&env);
} END_TEST

// variable with nested environment
START_TEST (test_EvalSimple_var2) {
    DynArr *pSExpList = parseSExps("var", stderr);
    ck_assert_ptr_ne(pSExpList, NULL);
    ck_assert_int_eq(dynArrCount(pSExpList), 1);

    SExp expect = { sexpInteger, { .integerContent = 12345 } };
    SExp another = { sexpInteger, { .integerContent = 11111 } };
    Environment env1 = {0};
    envInit(&env1);
    envInsert(&env1, "var", &expect);

    Environment env2 = {0};
    envInit(&env2);
    envInsert(&env2, "var", &another);
    envSetParent(&env2, &env1);

    SExp **pExp = dynArrBegin(pSExpList);
    const SExp *result = evVariable(*pExp, &env2);
    ck_assert_ptr_ne(result, NULL);
    ck_assert(isSExpEqual(&another,result));
    freeSExps(pSExpList);
    envFree(&env1);
    envFree(&env2);
} END_TEST

// test for lambda expressions is in EvalSetDefineSuite,
// as the special form of function definition is just a syntactic sugar
// of binding a lambda expression (object) to a variable

Suite * evalSimpleSuite(void) {
    Suite *s;
    TCase *tc_core;

    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_EvalSimple_int);
    tcase_add_test(tc_core, test_EvalSimple_boot_true);
    tcase_add_test(tc_core, test_EvalSimple_boot_false);
    tcase_add_test(tc_core, test_EvalSimple_str);
    tcase_add_test(tc_core, test_EvalSimple_quote1);
    tcase_add_test(tc_core, test_EvalSimple_quote2);
    tcase_add_test(tc_core, test_EvalSimple_var1);
    tcase_add_test(tc_core, test_EvalSimple_var2);

    s = suite_create("EvalSimple");
    suite_add_tcase(s, tc_core);
    return s;
}
