;; -*- lexical-binding: t -*-

(defun test-indent (indented &optional deindented)
  (let ((deindented (or deindented (replace-regexp-in-string "^[[:blank:]]*" "      " indented))))
    (rustic-test-manip-code
     deindented
     1
     (lambda ()
       (indent-region 1 (+ 1 (buffer-size))))
     indented)))


(ert-deftest indent-struct-fields-aligned ()
  (test-indent
   "
struct Foo { bar: i32,
             baz: i32 }

struct Blah {x:i32,
             y:i32,
             z:String"))

(ert-deftest indent-doc-comments ()
  (test-indent
   "
/**
 * This is a doc comment
 *
 */

/// So is this

fn foo() {
    /*!
     * this is a nested doc comment
     */
    \n    //! And so is this
}"))

(ert-deftest indent-inside-braces ()
  (test-indent
   "
// struct fields out one level:
struct foo {
    a:i32,
    // comments too
    b:char
}

fn bar(x:Box<i32>) {   // comment here should not affect the next indent
    bla();
    bla();
}"))

(ert-deftest indent-top-level ()
  (test-indent
   "
// Everything here is at the top level and should not be indented
#[attrib]
mod foo;

pub static bar = Quux{a: b()}

use foo::bar::baz;

fn foo() { }
"))

(ert-deftest font-lock-multi-raw-strings-in-a-row ()
  (rustic-test-font-lock
   "
r\"foo\\\", \"bar\", r\"bar\";
r\"foo\\.\", \"bar\", r\"bar\";
r\"foo\\..\", \"bar\", r\"foo\\..\\bar\";
r\"\\\", \"foo\", r\"\\foo\";
not_a_string();

"

   (apply 'append (mapcar (lambda (s) (list s 'font-lock-string-face))
                          '("r\"foo\\\"" "\"bar\"" "r\"bar\""
                            "r\"foo\\.\"" "\"bar\"" "r\"bar\""
                            "r\"foo\\..\"" "\"bar\"" "r\"foo\\..\\bar\""
                            "r\"\\\"" "\"foo\"" "r\"\\foo\"")))
   ))

(ert-deftest font-lock-raw-string-after-normal-string-ending-in-r ()
  (rustic-test-font-lock
   "\"bar\" r\"foo\""
   '("\"bar\"" font-lock-string-face "r\"foo\"" font-lock-string-face)))

(ert-deftest indent-params-no-align ()
  (test-indent
   "
// Indent out one level because no params appear on the first line
fn xyzzy(
    a:i32,
    b:char) { }

fn abcdef(
    a:i32,
    b:char)
    -> char
{ }"))

(ert-deftest indent-params-align ()
  (test-indent
   "
// Align the second line of params to the first
fn foo(a:i32,
       b:char) { }

fn bar(   a:i32,
          b:char)
          -> i32
{ }

fn baz(   a:i32,  // should work with a comment here
          b:char)
          -> i32
{ }
"))

(ert-deftest indent-open-after-arrow ()
  (test-indent
   "
// Indent function body only one level after `-> {`
fn foo1(a:i32, b:char) -> i32 {
    let body;
}

fn foo2(a:i32,
        b:char) -> i32 {
    let body;
}

fn foo3(a:i32,
        b:char)
        -> i32 {
    let body;
}

fn foo4(a:i32,
        b:char)
        -> i32 where i32:A {
    let body;
}
"))

(ert-deftest indent-body-after-where ()
  (let ((rustic-indent-where-clause t))
    (test-indent
     "
fn foo1(a: A, b: B) -> A
    where A: Clone + Default, B: Eq {
    let body;
    Foo {
        bar: 3
    }
}

fn foo2(a: A, b: B) -> A
    where A: Clone + Default, B: Eq
{
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-style1a ()
  (let ((rustic-indent-where-clause t))
    (test-indent
     "
fn foo1a(a: A, b: B, c: C) -> D
    where A: Clone + Default,
          B: Eq,
          C: PartialEq,
          D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-style1b ()
  (let ((rustic-indent-where-clause t))
    (test-indent
     "
fn foo1b(a: A, b: B, c: C) -> D
    where A: Clone + Default,
          B: Eq,
          C: PartialEq,
          D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-style2a ()
  (test-indent
   "
fn foo2a(a: A, b: B, c: C) -> D where A: Clone + Default,
                                      B: Eq,
                                      C: PartialEq,
                                      D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style2b ()
  (test-indent
   "
fn foo2b(a: A, b: B, c: C) -> D where A: Clone + Default,
                                      B: Eq,
                                      C: PartialEq,
                                      D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style3a ()
  (test-indent
   "
fn foo3a(a: A, b: B, c: C) -> D where
    A: Clone + Default,
    B: Eq,
    C: PartialEq,
    D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style3b ()
  (test-indent
   "
fn foo3b(a: A, b: B, c: C) -> D where
    A: Clone + Default,
    B: Eq,
    C: PartialEq,
    D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
"))

(ert-deftest indent-align-where-clauses-style4a ()
  (let ((rustic-indent-where-clause nil))
    (test-indent
     "
fn foo4a(a: A, b: B, c: C) -> D
where A: Clone + Default,
      B: Eq,
      C: PartialEq,
      D: PartialEq {
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-style4b ()
  (let ((rustic-indent-where-clause nil))
    (test-indent
     "
fn foo4b(a: A, b: B, c: C) -> D
where A: Clone + Default,
      B: Eq,
      C: PartialEq,
      D: PartialEq
{
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-impl-example ()
  (let ((rustic-indent-where-clause t))
    (test-indent
     "
impl<'a, K, Q: ?Sized, V, S> Index<&'a Q> for HashMap<K, V, S>
    where K: Eq + Hash + Borrow<Q>,
          Q: Eq + Hash,
          S: HashState,
{
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-clauses-first-line ()
  (let ((rustic-indent-where-clause t))
    (test-indent
     "fn foo1(a: A, b: B) -> A
    where A: Clone + Default, B: Eq {
    let body;
    Foo {
        bar: 3
    }
}
")))

(ert-deftest indent-align-where-in-comment1 ()
  (test-indent
   "/// - there must not exist an edge U->V in the graph where:
#[derive(Clone, PartialEq, Eq)]
pub struct Region { // <-- this should be flush with left margin!
    entry: BasicBlockIndex,
    leaves: BTreeMap<BasicBlockIndex, usize>,
}
"))

(ert-deftest indent-align-where-in-comment2 ()
  (let ((rustic-indent-where-clause t))
    (test-indent
     "fn foo<F,G>(f:F, g:G)
    where F:Send,
// where
          G:Sized
{
    let body;
}
")))

(ert-deftest indent-align-where-in-comment3 ()
  (let ((rustic-indent-where-clause t))
    (test-indent
     "fn foo<F,G>(f:F, g:G)
    where F:Send,
// where      F:ThisIsNotActualCode,
          G:Sized
{
    let body;
}
")))

(ert-deftest indent-square-bracket-alignment ()
  (test-indent
   "
fn args_on_the_next_line( // with a comment
    a:i32,
    b:String) {
    let aaaaaa = [
        1,
        2,
        3];
    let bbbbbbb = [1, 2, 3,
                   4, 5, 6];
    let ccc = [   10, 9, 8,
                  7, 6, 5];
}
"))

(ert-deftest indent-closing-square-bracket ()
  (test-indent
   "fn blergh() {
    let list = vec![
        1,
        2,
        3,
    ];
}"))

(ert-deftest indent-closing-paren ()
  (test-indent
   "fn blergh() {
    call(
        a,
        function
    );
}"))

(ert-deftest indent-nested-fns ()
  (test-indent
   "
fn nexted_fns(a: fn(b:i32,
                    c:char)
                    -> i32,
              d: i32)
              -> u128
{
    0
}
"
   ))

(ert-deftest indent-multi-line-expr ()
  (test-indent
   "
fn foo()
{
    x();
    let a =
        b();
}
"
   ))

(ert-deftest indent-match ()
  (test-indent
   "
fn foo() {
    match blah {
        Pattern => stuff(),
        _ => whatever
    }
}
"
   ))

(ert-deftest indent-match-multiline-pattern ()
  (test-indent
   "
fn foo() {
    match blah {
        Pattern |
        Pattern2 => {
            hello()
        },
        _ => whatever
    }
}
"
   ))

(ert-deftest indent-indented-match ()
  (test-indent
   "
fn foo() {
    let x =
        match blah {
            Pattern |
            Pattern2 => {
                hello()
            },
            _ => whatever
        };
    y();
}
"
   ))

(ert-deftest indent-curly-braces-within-parens ()
  (test-indent
   "
fn foo() {
    let x =
        foo(bar(|x| {
            only_one_indent_here();
        }));
    y();
}
"
   ))

(ert-deftest indent-weirdly-indented-block ()
  (rustic-test-manip-code
   "
fn foo() {
 {
this_block_is_over_to_the_left_for_some_reason();
 }

}
"
   16
   #'indent-for-tab-command
   "
fn foo() {
 {
     this_block_is_over_to_the_left_for_some_reason();
 }

}
"
   ))

(ert-deftest indent-multi-line-attrib ()
  (test-indent
   "
#[attrib(
    this,
    that,
    theotherthing)]
fn function_with_multiline_attribute() {}
"
   ))


;; Make sure that in effort to cover match patterns we don't mistreat || or expressions
(ert-deftest indent-nonmatch-or-expression ()
  (test-indent
   "
fn foo() {
    let x = foo() ||
        bar();
}
"
   ))

;; Closing braces in single char literals and strings should not confuse the indentation
(ert-deftest indent-closing-braces-in-char-literals ()
  (test-indent
   "
fn foo() {
    { bar('}'); }
    { bar(']'); }
    { bar(')'); }
}
"
   ))

;; This is a test for #103: a comment after the last struct member that does
;; not have a trailing comma. The comment used to be indented one stop too
;; far.
(ert-deftest indent-comment-after-last-struct-member ()
  (test-indent
   "
struct A {
    x: u8
    // comment
}

struct A {
    x: u8
    /* comment */
}
"
   ))

(ert-deftest indent-method-chains-no-align ()
  (let ((rustic-indent-method-chain nil)) (test-indent
                                           "
fn main() {
    let x = thing.do_it()
        .aligned()
        .more_alignment();
}
"
                                           )))

;; TODO: fix test
;; (ert-deftest indent-method-chains-no-align-with-question-mark-operator ()
;;   (let ((rustic-indent-method-chain nil)) (test-indent
;;    "
;; fn main() {
;;     let x = thing.do_it()
;;         .aligned()
;;         .more_alignment()?
;;         .more_alignment();
;; }
;; "
;;    )))

(ert-deftest indent-method-chains-with-align ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    let x = thing.do_it()
                 .aligned()
                 .more_alignment();
}
"
                                         )))

;; TODO: fix test
;; (ert-deftest indent-method-chains-with-align-with-question-mark-operator ()
;;   (let ((rustic-indent-method-chain t)) (test-indent
;;    "
;; fn main() {
;;     let x = thing.do_it()
;;                  .aligned()
;;                  .more_alignment()?
;;                  .more_alignment();
;; }
;; "
;;    )))

(ert-deftest indent-method-chains-with-align-and-second-stmt ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    let x = thing.do_it()
                 .aligned()
                 .more_alignment();
    foo.bar();
}
"
                                         )))

(ert-deftest indent-method-chains-field ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    let x = thing.do_it
                 .aligned
                 .more_alignment();
}
"
                                         )))

(ert-deftest indent-method-chains-double-field-on-first-line ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    let x = thing.a.do_it
                   .aligned
                   .more_alignment();
}
"
                                         )))

(ert-deftest indent-method-chains-no-let ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    thing.a.do_it
           .aligned
           .more_alignment();
}
"
                                         )))

(ert-deftest indent-method-chains-look-over-comment ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    thing.a.do_it
    // A comment
           .aligned
    // Another comment
           .more_alignment();
}
"
                                         )))

(ert-deftest indent-method-chains-comment ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    // thing.do_it()
    // .aligned()
}
"
                                         )))

(ert-deftest indent-method-chains-close-block ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    foo.bar()
}
"
                                         )))

(ert-deftest indent-method-chains-after-comment ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() { // comment here should not push next line out
    foo.bar()
}
"
                                         )))

(ert-deftest indent-method-chains-after-comment2 ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn main() {
    // Lorem ipsum lorem ipsum lorem ipsum lorem.ipsum
    foo.bar()
}
"
                                         )))

(ert-deftest indent-function-after-where ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn each_split_within<'a, F>(ss: &'a str, lim: usize, mut it: F)
                            -> bool where F: FnMut(&'a str) -> bool {
}

#[test]
fn test_split_within() {
}
"
                                         )))

(ert-deftest indent-function-after-where-nested ()
  (let ((rustic-indent-method-chain t)) (test-indent
                                         "
fn outer() {
    fn each_split_within<'a, F>(ss: &'a str, lim: usize, mut it: F)
                                -> bool where F: FnMut(&'a str) -> bool {
    }
    #[test]
    fn test_split_within() {
    }
    fn bar() {
    }
}
"
                                         )))


(ert-deftest test-indent-string-with-eol-backslash ()
  (test-indent
   "
pub fn foo() {
    format!(\"abc \\
             def\")
}
"
   ))

(ert-deftest test-indent-string-with-eol-backslash-at-start ()
  (test-indent
   "
pub fn foo() {
    format!(\"\\
        abc \\
        def\")
}
"
   ))

(ert-deftest test-indent-string-without-eol-backslash-indent-is-not-touched ()
  (test-indent
   "
pub fn foo() {
    format!(\"
abc
def\");
}

pub fn foo() {
    format!(\"la la la
la
la la\");
}
"
   ;; Should still indent the code parts but leave the string internals alone:
   "
         pub fn foo() {
    format!(\"
abc
def\");
}

pub fn foo() {
    format!(\"la la la
la
la la\");
   }
"
   ))

(ert-deftest test-indent-string-eol-backslash-mixed-with-literal-eol ()
  (test-indent
   "
fn foo() {
    println!(\"
Here is the beginning of the string
            and here is a line that is arbitrarily indented \\
            and a continuation of that indented line
     and another arbitrary indentation
  still another
        yet another \\
        with a line continuing it
And another line not indented
\")
}
"
   "
fn foo() {
    println!(\"
Here is the beginning of the string
            and here is a line that is arbitrarily indented \\
            and a continuation of that indented line
     and another arbitrary indentation
  still another
        yet another \\
with a line continuing it
And another line not indented
\")
}
"))

(ert-deftest test-indent-string-eol-backslash-dont-touch-raw-strings ()
  (test-indent
   "
pub fn foo() {
    format!(r\"\
abc\
         def\");
}

pub fn foo() {
    format!(r\"la la la
    la\
la la\");
}
"
   ;; Should still indent the code parts but leave the string internals alone:
   "
    pub fn foo() {
    format!(r\"\
abc\
         def\");
}

pub fn foo() {
          format!(r\"la la la
    la\
la la\");
}
"
   ))

(ert-deftest indent-inside-string-first-line ()
  (test-indent
   ;; Needs to leave 1 space before "world"
   "\"hello \\\n world\""))

(ert-deftest indent-multi-line-type-param-list ()
  (test-indent
   "
pub fn foo<T,
           V>() {
    hello();
}"))

(ert-deftest indent-open-paren-in-column0 ()
  ;; Just pass the same text for the "deindented" argument.  This
  ;; avoids the extra spaces normally inserted, which would mess up
  ;; the test because string contents aren't touched by reindentation.
  (let ((text "
const a: &'static str = r#\"
{}\"#;
fn main() {
    let b = \"//\";
    let c = \"\";

}
"))
    (test-indent text text)))

(ert-deftest indent-question-mark-operator ()
  (test-indent "fn foo() {
    if bar()? < 1 {
    }
    baz();
}"))

;; Regression test for #212.
(ert-deftest indent-left-shift ()
  (test-indent "
fn main() {
    let a = [[0u32, 0u32]; 1];
    let i = 0;
    let x = a[i][(1 < i)];
    let x = a[i][(1 << i)];
}
"))
