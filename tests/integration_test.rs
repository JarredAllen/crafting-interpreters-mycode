//! This file creates various functions in Lox, and then runs them and verifies that the output is
//! correct.

use rlox1::{Interpreter, NativeInterpreter, NativeLexer, NativeParser};

use std::fs::File;

#[test]
fn test_assert() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code("assert(true, \"Message\");").unwrap();
    interpreter.run_code("assert(false, \"Message\");").unwrap_err();
}

#[test]
fn test_arity_error() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    for snippet in ["assert();", "assert(true);", "print();", "print(5, 10);"].iter() {
        interpreter.run_code(snippet).unwrap_err();
    }
}

#[test]
fn test_static_checks() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    for snippet in ["return 5.0;", "if (false) { return 5.0; }", "fun foo() { return 5;; }", "class Foo { fun foo(){} fun foo(){} fun bar(){} }"].iter() {
        interpreter.run_code(snippet).unwrap_err();
    }
}

const FIBONACCI_FUNCTION_DEFN: &'static str = "fun fibonacci(n) {\n\tif n <= 1 {\n\t\treturn n;\n\t} else {\n\t\treturn fibonacci(n-1) + fibonacci(n-2);\n} }";

#[test]
fn test_fibonacci_code() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code(FIBONACCI_FUNCTION_DEFN).unwrap();
    for (index, answer) in [(0, 0), (1, 1), (2, 1), (3, 2), (4, 3), (5, 5), (6, 8), (7, 13), (8, 21), (9, 34), (10, 55), (11, 89)].iter() {
        let snippet = format!("assert(fibonacci({0}) == {1}, \"Failed at fib({0})\");", index, answer);
        interpreter.run_code(&snippet).unwrap();
    }
}

const SUM_SQUARES_DEFN: &'static str = "fun sum_squares(n) {\n\tvar sum = 0;;\n\tfor var i = 0; i = i+1; i <= n {\n\t\tvar square = i*i;\n\t\tsum = sum + square;\n\t}\n\treturn sum;\n}";

#[test]
fn test_sum_squares() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code(SUM_SQUARES_DEFN).unwrap();
    for (index, answer) in [(0, 0), (1, 1), (2, 5), (3, 14), (4, 30)].iter() {
        let snippet = format!("assert(sum_squares({0}) == {1}, \"Failed at sum_squares({0})\");", index, answer);
        interpreter.run_code(&snippet).unwrap();
    }
}

const COLLATZ_DEFN: &'static str = "fun collatz_step(n) {\n\tif n % 2 == 0 {\n\t\treturn n/2;\n\t} else {\n\t\treturn 3*n+1;\n\t}\n}\nfun collatz(n) {\n\tvar num_steps = 0;\n\twhile n != 1 {\n\t\tn = collatz_step(n);\n\t\tnum_steps = num_steps+1;\n\t}\n\treturn num_steps;\n}\n";

#[test]
fn test_collatz() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code(COLLATZ_DEFN).unwrap();
    for (index, answer) in [(1, 0), (2, 1), (4, 2), (27, 111)].iter() {
        let snippet = format!("assert(collatz({0}) == {1}, \"Failed at collatz({0})\");", index, answer);
        interpreter.run_code(&snippet).unwrap();
    }
}

#[test]
fn test_misc_oop() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code("var obj = new object(); obj.bar = 5; obj.baz = \"hello\"; assert(obj.bar == 5, \"\"); assert(obj.baz == \"hell\"+\"o\", \"\"); obj.bar = false; assert(!obj.bar, \"\");").unwrap();
    interpreter.run_code("class Foo { fun foo() { return \"foo\"; } }; var foo = new Foo();").unwrap();
    // interpreter.run_code("new Foo().foo(); foo.foo();").unwrap();
}

#[test]
fn test_booleans() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    for snippet in ["assert(!false, \"\");", "assert(true or false, \"\");", "assert(true and true, \"\");"].iter() {
        interpreter.run_code(snippet).unwrap();
    }
    for snippet in ["assert(!true, \"\");", "assert(true and false, \"\");", "assert(false or false, \"\");"].iter() {
        interpreter.run_code(snippet).unwrap_err();
    }
}

#[test]
fn test_lambdas() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code("assert(fun (x, y) { return -(x*y); }(2, 3) == -6, \"\"); // Test lambdas").unwrap();
}

const FACTORIAL_ITERATIVE_DEFN: &'static str = "fun factorial(x) {\n\tvar product = 1;\n\tfor var i = 1; i = i+1; i <= x\n\t{\n\t\tproduct = product * i;\n\t}\n\treturn product;\n}";
const FACTORIAL_RECURSIVE_DEFN: &'static str = "var factorial = fun(x) { if x == 0 { return 1; } else { return x*factorial(x-1); } };";

#[test]
fn test_factorials() {
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code(FACTORIAL_RECURSIVE_DEFN).unwrap();
    for (index, answer) in [(0, 1), (1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720), (7, 5040)].iter() {
        let snippet = format!("assert(factorial({0}) == {1}, \"Failed at factorial({0})\");", index, answer);
        interpreter.run_code(&snippet).unwrap();
    }
    let mut interpreter = NativeInterpreter::<NativeLexer, NativeParser>::new();
    interpreter.run_code(FACTORIAL_ITERATIVE_DEFN).unwrap();
    for (index, answer) in [(0, 1), (1, 1), (2, 2), (3, 6), (4, 24), (5, 120), (6, 720), (7, 5040)].iter() {
        let snippet = format!("assert(factorial({0}) == {1}, \"Failed at factorial({0})\");", index, answer);
        interpreter.run_code(&snippet).unwrap();
    }
    rlox1::run_file(File::open("tests/factorial.lox").unwrap()).unwrap();
}
