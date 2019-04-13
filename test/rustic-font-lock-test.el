;; -*- lexical-binding: t -*-

(defun rustic-test-fontify-string (str)
  (with-temp-buffer
    (rustic-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(ert-deftest rustic-test-font-lock-fontify-angle-brackets ()
    "Test that angle bracket fontify."
    (should (equal (rustic-test-fontify-string "<>") "<>"))
    (should (equal (rustic-test-fontify-string "<foo>") "<foo>"))
    (should (equal (rustic-test-fontify-string "<<>>") "<<>>"))
    (should (equal (rustic-test-fontify-string "<>>") "<>>"))
    (should (equal (rustic-test-fontify-string "<<>") "<<>")))

(defun rustic-test-font-lock (source face-groups)
  "Test that `SOURCE' fontifies to the expected `FACE-GROUPS'"
  (should (equal (rustic-test-group-str-by-face source)
                 face-groups)))

(defun rustic-test-fontify-string (str)
  (with-temp-buffer
    (rustic-mode)
    (insert str)
    (font-lock-fontify-buffer)
    (buffer-string)))

(ert-deftest font-lock-attribute-simple ()
  (rustic-test-font-lock
   "#[foo]"
   '("#[foo]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-inner ()
  (rustic-test-font-lock
   "#![foo]"
   '("#![foo]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-key-value ()
  (rustic-test-font-lock
   "#[foo = \"bar\"]"
   '("#[foo = " font-lock-preprocessor-face
     "\"bar\"" font-lock-string-face
     "]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-around-comment ()
  (rustic-test-font-lock
   "#[foo /* bar */]"
   '("#[foo " font-lock-preprocessor-face
     "/* " font-lock-comment-delimiter-face
     "bar */" font-lock-comment-face
     "]" font-lock-preprocessor-face)))

(ert-deftest font-lock-attribute-inside-string ()
  (rustic-test-font-lock
   "\"#[foo]\""
   '("\"#[foo]\"" font-lock-string-face)))

(ert-deftest font-lock-attribute-inside-comment ()
  (rustic-test-font-lock
   "/* #[foo] */"
   '("/* " font-lock-comment-delimiter-face
     "#[foo] */" font-lock-comment-face)))

(ert-deftest font-lock-double-quote-character-literal ()
  (rustic-test-font-lock
   "'\"'; let"
   '("'\"'" font-lock-string-face
     "let" font-lock-keyword-face)))

(ert-deftest font-lock-fn-contains-capital ()
  (rustic-test-font-lock
   "fn foo_Bar() {}"
   '("fn" font-lock-keyword-face
     "foo_Bar" font-lock-function-name-face)))

(ert-deftest font-lock-let-bindings ()
  (rustic-test-font-lock
   "let foo;"
   '("let" font-lock-keyword-face
     "foo" font-lock-variable-name-face))
  (rustic-test-font-lock
   "let mut foo;"
   '("let" font-lock-keyword-face
     "mut" font-lock-keyword-face
     "foo" font-lock-variable-name-face))
  (rustic-test-font-lock
   "let foo = 1;"
   '("let" font-lock-keyword-face
     "foo" font-lock-variable-name-face))
  (rustic-test-font-lock
   "let mut foo = 1;"
   '("let" font-lock-keyword-face
     "mut" font-lock-keyword-face
     "foo" font-lock-variable-name-face))
  (rustic-test-font-lock
   "fn foo() { let bar = 1; }"
   '("fn" font-lock-keyword-face
     "foo" font-lock-function-name-face
     "let" font-lock-keyword-face
     "bar" font-lock-variable-name-face))
  (rustic-test-font-lock
   "fn foo() { let mut bar = 1; }"
   '("fn" font-lock-keyword-face
     "foo" font-lock-function-name-face
     "let" font-lock-keyword-face
     "mut" font-lock-keyword-face
     "bar" font-lock-variable-name-face)))

(ert-deftest font-lock-if-let-binding ()
  (rustic-test-font-lock
   "if let Some(var) = some_var { /* no-op */ }"
   '("if" font-lock-keyword-face
     "let" font-lock-keyword-face
     "Some" font-lock-type-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face)))

(ert-deftest font-lock-single-quote-character-literal ()
  (rustic-test-font-lock
   "fn main() { let ch = '\\''; }"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "let" font-lock-keyword-face
     "ch" font-lock-variable-name-face
     "'\\''" font-lock-string-face)))

(ert-deftest font-lock-escaped-double-quote-character-literal ()
  (rustic-test-font-lock
   "fn main() { let ch = '\\\"'; }"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "let" font-lock-keyword-face
     "ch" font-lock-variable-name-face
     "'\\\"'" font-lock-string-face)))

(ert-deftest font-lock-escaped-backslash-character-literal ()
  (rustic-test-font-lock
   "fn main() { let ch = '\\\\'; }"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "let" font-lock-keyword-face
     "ch" font-lock-variable-name-face
     "'\\\\'" font-lock-string-face)))

(ert-deftest font-lock-hex-escape-character-literal ()
  (rustic-test-font-lock
   "let ch = '\\x1f';"
   '("let" font-lock-keyword-face
     "ch" font-lock-variable-name-face
     "'\\x1f'" font-lock-string-face)))

(ert-deftest font-lock-unicode-escape-character-literal ()
  (rustic-test-font-lock
   "let ch = '\\u{1ffff}';"
   '("let" font-lock-keyword-face
     "ch" font-lock-variable-name-face
     "'\\u{1ffff}'" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-no-hashes ()
  (rustic-test-font-lock
   "r\"No hashes\";"
   '("r\"No hashes\"" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-double-quote ()
  (rustic-test-font-lock
   "fn main() {
    r#\"With a double quote (\")\"#;
}
"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "r#\"With a double quote (\")\"#" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-two-hashes ()
  (rustic-test-font-lock
   "r##\"With two hashes\"##;"
   '("r##\"With two hashes\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-strings-backslash-at-end ()
  (rustic-test-font-lock
   "r\"With a backslash at the end\\\";"
   '("r\"With a backslash at the end\\\"" font-lock-string-face)))

(ert-deftest font-lock-two-raw-strings ()
  (rustic-test-font-lock
   "fn main() {
    r\"With a backslash at the end\\\";
    r##\"With two hashes\"##;
}"
   '("fn" font-lock-keyword-face
     "main" font-lock-function-name-face
     "r\"With a backslash at the end\\\"" font-lock-string-face
     "r##\"With two hashes\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-with-inner-hash ()
  (rustic-test-font-lock
   "r##\"I've got an octothorpe (#)\"##; foo()"
   '("r##\"I've got an octothorpe (#)\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-with-inner-quote-and-hash ()
  (rustic-test-font-lock
   "not_the_string(); r##\"string \"# still same string\"##; not_the_string()"
   '("r##\"string \"# still same string\"##" font-lock-string-face)))

(ert-deftest font-lock-string-ending-with-r-not-raw-string ()
  (rustic-test-font-lock
   "fn f() {
    \"Er\";
}

fn g() {
    \"xs\";
}"
   '("fn" font-lock-keyword-face
     "f" font-lock-function-name-face
     "\"Er\"" font-lock-string-face
     "fn" font-lock-keyword-face
     "g" font-lock-function-name-face
     "\"xs\"" font-lock-string-face)))


(ert-deftest font-lock-raw-string-trick-ending-followed-by-string-with-quote ()
  (rustic-test-font-lock
   "r\"With what looks like the start of a raw string at the end r#\";
not_a_string();
r##\"With \"embedded\" quote \"##;"
   '("r\"With what looks like the start of a raw string at the end r#\"" font-lock-string-face
     "r##\"With \"embedded\" quote \"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-starter-inside-raw-string ()
  ;; Check that it won't look for a raw string beginning inside another raw string.
  (rustic-test-font-lock
   "r#\"In the first string r\" in the first string \"#;
not_in_a_string();
r##\"In the second string\"##;"
   '("r#\"In the first string r\" in the first string \"#" font-lock-string-face
     "r##\"In the second string\"##" font-lock-string-face)))

(ert-deftest font-lock-raw-string-starter-inside-comment ()
  ;; Check that it won't look for a raw string beginning inside another raw string.
  (rustic-test-font-lock
   "// r\" this is a comment
\"this is a string\";
this_is_not_a_string();)"
   '("// " font-lock-comment-delimiter-face
     "r\" this is a comment\n" font-lock-comment-face
     "\"this is a string\"" font-lock-string-face)))

(ert-deftest font-lock-runaway-raw-string ()
  (rustic-test-font-lock
   "const Z = r#\"my raw string\";\n// oops this is still in the string"
   '("const" font-lock-keyword-face
     "Z" font-lock-type-face
     "r#\"my raw string\";\n// oops this is still in the string" font-lock-string-face))
  )

(ert-deftest font-lock-recognize-closing-raw-string ()
  (with-temp-buffer
    (rustic-mode)
    (insert "const foo = r##\"
1...............................................50
1...............................................50
1...............................................50
1...............195-->\"; let ...................50
1...............................................50
1...............................................50
1...............................................50
1...............................................50
1...............................................50
1......................500......................50
\"#;
")
    (font-lock-fontify-buffer)
    (goto-char 530)
    (insert "#")
    ;; We have now closed the raw string.  Check that the whole string is
    ;; recognized after the change
    (font-lock-after-change-function (1- (point)) (point) 0)
    (should (equal 'font-lock-string-face (get-text-property 195 'face))) ;; The "let"
    (should (equal 'font-lock-string-face (get-text-property 500 'face))) ;; The "500"
    (should (equal nil (get-text-property 531 'face))) ;; The second ";"
    ))

;;; Documentation comments

(ert-deftest font-lock-doc-line-comment-parent ()
  (rustic-test-font-lock
   "//! doc"
   '("//! doc" font-lock-doc-face)))

(ert-deftest font-lock-doc-line-comment-item ()
  (rustic-test-font-lock
   "/// doc"
   '("/// doc" font-lock-doc-face)))

(ert-deftest font-lock-nondoc-line ()
  (rustic-test-font-lock
   "////// doc"
   '("////// " font-lock-comment-delimiter-face
     "doc" font-lock-comment-face)))

(ert-deftest font-lock-doc-line-in-string ()
  (rustic-test-font-lock
   "\"/// doc\""
   '("\"/// doc\"" font-lock-string-face))

  (rustic-test-font-lock
   "\"//! doc\""
   '("\"//! doc\"" font-lock-string-face)))

(ert-deftest font-lock-doc-line-in-nested-comment ()
  (rustic-test-font-lock
   "/* /// doc */"
   '("/* " font-lock-comment-delimiter-face
     "/// doc */" font-lock-comment-face))

  (rustic-test-font-lock
   "/* //! doc */"
   '("/* " font-lock-comment-delimiter-face
     "//! doc */" font-lock-comment-face)))


(ert-deftest font-lock-doc-block-comment-parent ()
  (rustic-test-font-lock
   "/*! doc */"
   '("/*! doc */" font-lock-doc-face)))

(ert-deftest font-lock-doc-block-comment-item ()
  (rustic-test-font-lock
   "/** doc */"
   '("/** doc */" font-lock-doc-face)))

(ert-deftest font-lock-nondoc-block-comment-item ()
  (rustic-test-font-lock
   "/***** doc */"
   '("/**" font-lock-comment-delimiter-face
     "*** doc */" font-lock-comment-face)))

(ert-deftest font-lock-doc-block-in-string ()
  (rustic-test-font-lock
   "\"/** doc */\""
   '("\"/** doc */\"" font-lock-string-face))
  (rustic-test-font-lock
   "\"/*! doc */\""
   '("\"/*! doc */\"" font-lock-string-face)))

(ert-deftest font-lock-module-def ()
  (rustic-test-font-lock
   "mod foo;"
   '("mod" font-lock-keyword-face
     "foo" font-lock-constant-face)))

(ert-deftest font-lock-module-use ()
  (rustic-test-font-lock
   "use foo;"
   '("use" font-lock-keyword-face
     "foo" font-lock-constant-face)))

(ert-deftest font-lock-module-path ()
  (rustic-test-font-lock
   "foo::bar"
   '("foo" font-lock-constant-face)))

(ert-deftest font-lock-submodule-path ()
  (rustic-test-font-lock
   "foo::bar::baz"
   '("foo" font-lock-constant-face
     "bar" font-lock-constant-face)))

(ert-deftest font-lock-type ()
  (rustic-test-font-lock
   "foo::Bar::baz"
   '("foo" font-lock-constant-face
     "Bar" font-lock-type-face)))

(ert-deftest font-lock-type-annotation ()
  "Ensure type annotations are not confused with modules."
  (rustic-test-font-lock
   "parse::<i32>();"
   ;; Only the i32 should have been highlighted.
   '("i32" font-lock-type-face))
  (rustic-test-font-lock
   "foo:: <i32>"
   ;; Only the i32 should have been highlighted.
   '("i32" font-lock-type-face)))

(ert-deftest font-lock-question-mark ()
  "Ensure question mark operator is highlighted."
  (rustic-test-font-lock
   "?"
   '("?" rustic-question-mark-face))
  (rustic-test-font-lock
   "foo\(\)?;"
   '("?" rustic-question-mark-face))
  (rustic-test-font-lock
   "foo\(bar\(\)?\);"
   '("?" rustic-question-mark-face))
  (rustic-test-font-lock
   "\"?\""
   '("\"?\"" font-lock-string-face))
  (rustic-test-font-lock
   "foo\(\"?\"\);"
   '("\"?\"" font-lock-string-face))
  (rustic-test-font-lock
   "// ?"
   '("// " font-lock-comment-delimiter-face
     "?" font-lock-comment-face))
  (rustic-test-font-lock
   "/// ?"
   '("/// ?" font-lock-doc-face))
  (rustic-test-font-lock
   "foo\(\"?\"\);"
   '("\"?\"" font-lock-string-face))
  (rustic-test-font-lock
   "foo\(\"?\"\)?;"
   '("\"?\"" font-lock-string-face
     "?" rustic-question-mark-face)))

(ert-deftest rustic-test-default-context-sensitive ()
  (rustic-test-font-lock
   "let default = 7; impl foo { default fn f() { } }"
   '("let" font-lock-keyword-face
     "default" font-lock-variable-name-face
     "impl" font-lock-keyword-face
     "default" font-lock-keyword-face
     "fn" font-lock-keyword-face
     "f" font-lock-function-name-face)))

(ert-deftest rustic-test-union-context-sensitive ()
  (rustic-test-font-lock
   "let union = 7; union foo { x: &'union bar }"
   '("let" font-lock-keyword-face
     ;; The first union is a variable name.
     "union" font-lock-variable-name-face
     ;; The second union is a contextual keyword.
     "union" font-lock-keyword-face
     "foo" font-lock-type-face
     "x" font-lock-variable-name-face
     ;; This union is the name of a lifetime.
     "union" font-lock-variable-name-face
     "bar" font-lock-type-face)))

(ert-deftest single-quote-null-char ()
  (rustic-test-font-lock
   "'\\0' 'a' fn"
   '("'\\0'" font-lock-string-face
     "'a'" font-lock-string-face
     "fn" font-lock-keyword-face)))

(ert-deftest r-in-string-after-single-quoted-double-quote ()
  (rustic-test-font-lock
   "'\"';\n\"r\";\n\"oops\";"
   '("'\"'" font-lock-string-face
     "\"r\"" font-lock-string-face
     "\"oops\"" font-lock-string-face
     )))

(ert-deftest char-literal-after-quote-in-raw-string ()
  (rustic-test-font-lock
   "r#\"\"\"#;\n'q'"
   '("r#\"\"\"#" font-lock-string-face
     "'q'" font-lock-string-face)))

(ert-deftest rustic-macro-font-lock ()
  (rustic-test-font-lock
   "foo!\(\);"
   '("foo!" font-lock-preprocessor-face))
  (rustic-test-font-lock
   "foo!{};"
   '("foo!" font-lock-preprocessor-face))
  (rustic-test-font-lock
   "foo![];"
   '("foo!" font-lock-preprocessor-face)))

(ert-deftest rustic-write-macro-font-lock ()
  (rustic-test-font-lock
   "write!(f, \"abcd {0}}} efgh {1}\", foo, bar); { /* no-op */ }"
   '("write!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     "}} efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  (rustic-test-font-lock
   "writeln!(f, \"abcd {0}}} efgh {1}\", foo, bar); { /* no-op */ }"
   '("writeln!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     "}} efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face)))

;; TODO: fix test
(ert-deftest rustic-formatting-macro-font-lock ()
  ;; test that the block delimiters aren't highlighted and the comment
  ;; is ignored
  (rustic-test-font-lock
   "print!(\"\"); { /* print!(\"\"); */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "print!(\"\"); */" font-lock-comment-face))
  ;; ;; with newline directly following delimiter
  ;; (rustic-test-font-lock
  ;;  "print!(\n\"\"\n); { /* print!(\"\"); */ }"
  ;;  '("print!" rustic-builtin-formatting-macro-face
  ;;    "\"\"" font-lock-string-face
  ;;    "/* " font-lock-comment-delimiter-face
  ;;    "print!(\"\"); */" font-lock-comment-face))
  ;; ;; with empty println!()
  ;; (rustic-test-font-lock
  ;;  "println!(); { /* println!(); */ }"
  ;;  '("println!" rustic-builtin-formatting-macro-face
  ;;    "/* " font-lock-comment-delimiter-face
  ;;    "println!(); */" font-lock-comment-face))
  (rustic-test-font-lock
   "let ref foo;"
   '("let" font-lock-keyword-face
     "ref" font-lock-keyword-face
     "foo" font-lock-variable-name-face))  
  ;; other delimiters
  (rustic-test-font-lock
   "print!{\"\"}; { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; other delimiters
  (rustic-test-font-lock
   "print![\"\"]; { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; no interpolation
  (rustic-test-font-lock
   "print!(\"abcd\"); { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"abcd\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; only interpolation
  (rustic-test-font-lock
   "print!(\"{}\"); { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"" font-lock-string-face
     "{}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; text + interpolation
  (rustic-test-font-lock
   "print!(\"abcd {}\", foo); { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; text + interpolation with specification
  (rustic-test-font-lock
   "print!(\"abcd {0}\", foo); { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; text + interpolation with specification and escape
  (rustic-test-font-lock
   "print!(\"abcd {0}}}\", foo); { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     "}}\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; multiple pairs
  (rustic-test-font-lock
   "print!(\"abcd {0} efgh {1}\", foo, bar); { /* no-op */ }"
   '("print!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; println
  (rustic-test-font-lock
   "println!(\"abcd {0} efgh {1}\", foo, bar); { /* no-op */ }"
   '("println!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; eprint
  (rustic-test-font-lock
   "eprint!(\"abcd {0} efgh {1}\", foo, bar); { /* no-op */ }"
   '("eprint!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; eprintln
  (rustic-test-font-lock
   "eprintln!(\"abcd {0} efgh {1}\", foo, bar); { /* no-op */ }"
   '("eprintln!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; format
  (rustic-test-font-lock
   "format!(\"abcd {0} efgh {1}\", foo, bar); { /* no-op */ }"
   '("format!" rustic-builtin-formatting-macro-face
     "\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; print + raw string
  (rustic-test-font-lock
   "format!(r\"abcd {0} efgh {1}\", foo, bar); { /* no-op */ }"
   '("format!" rustic-builtin-formatting-macro-face
     "r\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; print + raw string with hash
  (rustic-test-font-lock
   "format!(r#\"abcd {0} efgh {1}\"#, foo, bar); { /* no-op */ }"
   '("format!" rustic-builtin-formatting-macro-face
     "r#\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"#" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face))
  ;; print + raw string with two hashes
  (rustic-test-font-lock
   "format!(r##\"abcd {0} efgh {1}\"##, foo, bar); { /* no-op */ }"
   '("format!" rustic-builtin-formatting-macro-face
     "r##\"abcd " font-lock-string-face
     "{0}" rustic-string-interpolation-face
     " efgh " font-lock-string-face
     "{1}" rustic-string-interpolation-face
     "\"##" font-lock-string-face
     "/* " font-lock-comment-delimiter-face
     "no-op */" font-lock-comment-face)))
