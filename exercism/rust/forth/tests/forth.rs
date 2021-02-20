use forth::{Error, Forth, Value};

#[test]
fn no_input_no_stack() {
    assert_eq!(Vec::<Value>::new(), Forth::new().stack());
}

#[test]
fn numbers_just_get_pushed_onto_the_stack() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 3 4 5").is_ok());
    assert_eq!(vec![1, 2, 3, 4, 5], f.stack());
}

#[test]
fn can_add_two_numbers() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 +").is_ok());
    assert_eq!(vec![3], f.stack());
}

#[test]
fn addition_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("1 +"));
    assert_eq!(Err(Error::StackUnderflow), f.eval("+"));
}

#[test]
fn can_subtract_two_numbers() {
    let mut f = Forth::new();
    assert!(f.eval("3 4 -").is_ok());
    assert_eq!(vec![-1], f.stack());
}

#[test]
fn subtraction_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("1 -"));
    assert_eq!(Err(Error::StackUnderflow), f.eval("-"));
}

#[test]
fn can_multiply_two_numbers() {
    let mut f = Forth::new();
    assert!(f.eval("2 4 *").is_ok());
    assert_eq!(vec![8], f.stack());
}

#[test]
fn multiplication_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("1 *"));
    assert_eq!(Err(Error::StackUnderflow), f.eval("*"));
}

#[test]
fn can_divide_two_numbers() {
    let mut f = Forth::new();
    assert!(f.eval("12 3 /").is_ok());
    assert_eq!(vec![4], f.stack());
}

#[test]
fn performs_integer_division() {
    let mut f = Forth::new();
    assert!(f.eval("8 3 /").is_ok());
    assert_eq!(vec![2], f.stack());
}

#[test]
fn division_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("1 /"));
    assert_eq!(Err(Error::StackUnderflow), f.eval("/"));
}

#[test]
fn errors_if_dividing_by_zero() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::DivisionByZero), f.eval("4 0 /"));
}

#[test]
fn addition_and_subtraction() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 + 4 -").is_ok());
    assert_eq!(vec![-1], f.stack());
}

#[test]
fn multiplication_and_division() {
    let mut f = Forth::new();
    assert!(f.eval("2 4 * 3 /").is_ok());
    assert_eq!(vec![2], f.stack());
}

#[test]
fn dup() {
    let mut f = Forth::new();
    assert!(f.eval("1 dup").is_ok());
    assert_eq!(vec![1, 1], f.stack());
}

#[test]
fn dup_top_value_only() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 dup").is_ok());
    assert_eq!(vec![1, 2, 2], f.stack());
}

#[test]
fn dup_case_insensitive() {
    let mut f = Forth::new();
    assert!(f.eval("1 DUP Dup dup").is_ok());
    assert_eq!(vec![1, 1, 1, 1], f.stack());
}

#[test]
fn dup_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("dup"));
}

#[test]
fn drop() {
    let mut f = Forth::new();
    assert!(f.eval("1 drop").is_ok());
    assert_eq!(Vec::<Value>::new(), f.stack());
}

#[test]
fn drop_with_two() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 drop").is_ok());
    assert_eq!(vec![1], f.stack());
}

#[test]
fn drop_case_insensitive() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 3 4 DROP Drop drop").is_ok());
    assert_eq!(vec![1], f.stack());
}

#[test]
fn drop_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("drop"));
}

#[test]
fn swap() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 swap").is_ok());
    assert_eq!(vec![2, 1], f.stack());
}

#[test]
fn swap_with_three() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 3 swap").is_ok());
    assert_eq!(vec![1, 3, 2], f.stack());
}

#[test]
fn swap_case_insensitive() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 SWAP 3 Swap 4 swap").is_ok());
    assert_eq!(vec![2, 3, 4, 1], f.stack());
}

#[test]
fn swap_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("1 swap"));
    assert_eq!(Err(Error::StackUnderflow), f.eval("swap"));
}

#[test]
fn over() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 over").is_ok());
    assert_eq!(vec![1, 2, 1], f.stack());
}

#[test]
fn over_with_three() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 3 over").is_ok());
    assert_eq!(vec![1, 2, 3, 2], f.stack());
}

#[test]
fn over_case_insensitive() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 OVER Over over").is_ok());
    assert_eq!(vec![1, 2, 1, 2, 1], f.stack());
}

#[test]
fn over_error() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::StackUnderflow), f.eval("1 over"));
    assert_eq!(Err(Error::StackUnderflow), f.eval("over"));
}

// User-defined words

#[test]
fn can_consist_of_built_in_words() {
    let mut f = Forth::new();
    assert!(f.eval(": dup-twice dup dup ;").is_ok());
    assert!(f.eval("1 dup-twice").is_ok());
    assert_eq!(vec![1, 1, 1], f.stack());
}

#[test]
fn execute_in_the_right_order() {
    let mut f = Forth::new();
    assert!(f.eval(": countup 1 2 3 ;").is_ok());
    assert!(f.eval("countup").is_ok());
    assert_eq!(vec![1, 2, 3], f.stack());
}

#[test]
fn redefining_an_existing_word() {
    let mut f = Forth::new();
    assert!(f.eval(": foo dup ;").is_ok());
    assert!(f.eval(": foo dup dup ;").is_ok());
    assert!(f.eval("1 foo").is_ok());
    assert_eq!(vec![1, 1, 1], f.stack());
}

#[test]
fn redefining_an_existing_built_in_word() {
    let mut f = Forth::new();
    assert!(f.eval(": swap dup ;").is_ok());
    assert!(f.eval("1 swap").is_ok());
    assert_eq!(vec![1, 1], f.stack());
}

#[test]
fn user_defined_words_are_case_insensitive() {
    let mut f = Forth::new();
    assert!(f.eval(": foo dup ;").is_ok());
    assert!(f.eval("1 FOO Foo foo").is_ok());
    assert_eq!(vec![1, 1, 1, 1], f.stack());
}

#[test]
fn definitions_are_case_insensitive() {
    let mut f = Forth::new();
    assert!(f.eval(": SWAP DUP Dup dup ;").is_ok());
    assert!(f.eval("1 swap").is_ok());
    assert_eq!(vec![1, 1, 1, 1], f.stack());
}

#[test]
fn redefining_a_built_in_operator() {
    let mut f = Forth::new();
    assert!(f.eval(": + * ;").is_ok());
    assert!(f.eval("3 4 +").is_ok());
    assert_eq!(vec![12], f.stack());
}

#[test]
fn can_use_different_words_with_the_same_name() {
    let mut f = Forth::new();
    assert!(f.eval(": foo 5 ;").is_ok());
    assert!(f.eval(": bar foo ;").is_ok());
    assert!(f.eval(": foo 6 ;").is_ok());
    assert!(f.eval("bar foo").is_ok());
    assert_eq!(vec![5, 6], f.stack());
}

#[test]
fn can_define_word_that_uses_word_with_the_same_name() {
    let mut f = Forth::new();
    assert!(f.eval(": foo 10 ;").is_ok());
    assert!(f.eval(": foo foo 1 + ;").is_ok());
    assert!(f.eval("foo").is_ok());
    assert_eq!(vec![11], f.stack());
}

#[test]
fn defining_a_number() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::InvalidWord), f.eval(": 1 2 ;"));
}

#[test]
fn malformed_word_definition() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::InvalidWord), f.eval(":"));
    assert_eq!(Err(Error::InvalidWord), f.eval(": foo"));
    assert_eq!(Err(Error::InvalidWord), f.eval(": foo 1"));
}

#[test]
fn calling_non_existing_word() {
    let mut f = Forth::new();
    assert_eq!(Err(Error::UnknownWord), f.eval("1 foo"));
}

#[test]
fn multiple_definitions() {
    let mut f = Forth::new();
    assert!(f.eval(": one 1 ; : two 2 ; one two +").is_ok());
    assert_eq!(vec![3], f.stack());
}

#[test]
fn definitions_after_ops() {
    let mut f = Forth::new();
    assert!(f.eval("1 2 + : addone 1 + ; addone").is_ok());
    assert_eq!(vec![4], f.stack());
}

#[test]
fn redefine_an_existing_word_with_another_existing_word() {
    let mut f = Forth::new();
    assert!(f.eval(": foo 5 ;").is_ok());
    assert!(f.eval(": bar foo ;").is_ok());
    assert!(f.eval(": foo 6 ;").is_ok());
    assert!(f.eval(": bar foo ;").is_ok());
    assert!(f.eval("bar foo").is_ok());
    assert_eq!(vec![6, 6], f.stack());
}
