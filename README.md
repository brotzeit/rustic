# Rustic

[![MELPA](https://melpa.org/packages/rustic-badge.svg)](https://melpa.org/#/rustic)
[![](https://github.com/brotzeit/rustic/workflows/CI/badge.svg)](https://github.com/brotzeit/rustic/actions?query=workflow%3ACI)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Rustic](#rustic)
    - [Intro](#intro)
    - [Known issues](#known-issues)
    - [Installation](#installation)
        - [straight](#straight)
    - [remote](#remote)
    - [Compilation](#compilation)
        - [Navigating errors](#navigating-errors)
        - [default directory](#default-directory)
        - [Faces](#faces)
        - [rustc errors](#rustc-errors)
    - [Rustfmt](#rustfmt)
        - [Change default arguments](#change-default-arguments)
        - [edition 2018](#edition-2018)
        - [remote](#remote-1)
    - [LSP](#lsp)
        - [Server](#server)
        - [Client](#client)
            - [eglot](#eglot)
            - [lsp-mode](#lsp-mode)
                - [`lsp-execute-code-action`](#lsp-execute-code-action)
                    - [Applying code actions](#applying-code-actions)
                    - [Auto import](#auto-import)
                - [Macro expansion](#macro-expansion)
        - [LSP + TRAMP](#lsp--tramp)
        - [Detached file](#detached-file)
    - [Cargo](#cargo)
        - [Edit](#edit)
        - [Test](#test)
        - [Run](#run)
        - [Outdated](#outdated)
        - [Expand](#expand)
        - [More cargo commands](#more-cargo-commands)
    - [Clippy](#clippy)
        - [auto-fixing before compilation](#auto-fixing-before-compilation)
        - [Commands](#commands)
        - [Flycheck](#flycheck)
        - [lsp-mode](#lsp-mode-1)
    - [Org-babel](#org-babel)
        - [Intro](#intro-1)
        - [lsp-mode](#lsp-mode-2)
        - [Commands](#commands-1)
        - [Parameters](#parameters)
            - [:crates](#crates)
            - [:features](#features)
            - [:paths](#paths)
            - [:toolchain](#toolchain)
            - [:main](#main)
            - [:include](#include)
            - [:use](#use)
    - [Spinner](#spinner)
    - [rust docs in org-mode](#rust-docs-in-org-mode)
        - [Prerequisites](#prerequisites)
        - [Usage](#usage)
        - [Notes](#notes)
    - [Popup](#popup)
    - [rust-mode](#rust-mode)
    - [elisp tests](#elisp-tests)
    - [Contributing](#contributing)

<!-- markdown-toc end -->


## Intro

This package is based on [rust-mode](https://github.com/rust-lang/rust-mode) and provides additional features:

- cargo popup
- multiline error parsing
- translation of ANSI control sequences through
  [xterm-color](https://github.com/atomontage/xterm-color)
- async org babel
- automatic LSP configuration with
  [eglot](https://github.com/joaotavora/eglot) or
  [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
- cask for testing
- etc.

rustic only shares the rust-mode code from rust-mode.el and rust-utils.el.
The other files provide functionality that is similar to some of the features
of rustic, however can be considered light-weight compared to some rustic's
functionality.

The shared functions and options exist as aliases in the rust-mode and
rustic namespace for backwards compatability reasons(rustic has been a fork).

## Known issues

- `rust-syntax-propertize` and `adaptive-wrap-prefix-mode` can lead to
  severe lag when editing larger files (#107)

## Installation

Simply put `(use-package rustic)` in your config and most stuff gets
configured automatically.
([use-package](https://github.com/jwiegley/use-package))

It's necessary to include elpa for a package dependency:

```elisp
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
```

If ‘spinner-1.7.3’ is unavailable” when trying to install rustic, you
need to update GPG keys used by the ELPA package manager. Try
installing
[gnu-elpa-keyring-update](https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html).

If you can't run rust-analyzer or cargo can't be found, your
environment variables probably don't work in emacs.  Try
[exec-path-from-shell](https://github.com/purcell/exec-path-from-shell/tree/81125c5adbc903943c016c2984906dc089372a41#usage)
to fix this.

### straight

[straight.el](https://github.com/raxod502/straight.el#install-packages)
clones each of your packages directly from its source. There are good
additional [installation
instructions](https://github.crookster.org/switching-to-straight.el-from-emacs-26-builtin-package.el/)
for moving your package management from package.el to straight.

## remote

rustfmt and most of the common cargo commands should work remotely.
We are currently updating the code base. If you encounter any command
that doesn't work remotely, please open an issue.

## Compilation

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/compilation_buffer.png)

Commands:

- `rustic-compile`            compile project using `rustic-compile-command`
- `rustic-recompile`          recompile using `compilation-arguments`
- `rustic-compile-send-input` send string to process of current buffer

Customization:

- `rustic-compile-display-method` choose function that displays the
  compilation buffer (use the function `ignore`, if you don't want the
  buffer to be displayed)
- `rustic-compile-backtrace` change backtrace verbosity
- `rustic-compile-rustflags` set RUSTFLAGS
- `rustic-compile-command` default command for rust compilation
- `rustic-compile-command-remote` default command for remote rust compilation

Supported compile.el variables:

- compilation-arguments
- compilation-scroll-output

### Navigating errors

Rustic defines a derived compilation-mode. Colors can be customized
with several defcustoms.  You can use `next-error` and
`compilation-next-error` as for any other compilation buffer.

However it's possible to also jump to line numbers that are displayed
at the beginning of a line.  This feature is provided by a hook around
`compile-goto-error`(`RET`).

### default directory

`rustic-compile-directory-method` allows you to set the directory that
is used for compilation commands. The default is the current crate
which is returned by `rustic-buffer-crate`(there's also
`rustic-buffer-workspace`).

If you want to use the project root you can use `rustic-project-root`
instead.

FTR #174 #179 #236

### Faces

The colors that are displayed in compilation buffers come from cargo
and are translated by xterm-color. You can change these colors by
modifying `rustic-ansi-faces`.

`rustic-compilation-mode` doesn't use the default faces of
compile.el. If you want to change these colors you can use something
similar to:

```elisp
(custom-set-faces
  '(rustic-compilation-column ((t (:inherit compilation-column-number))))
  '(rustic-compilation-line ((t (:foreground "LimeGreen")))))
```

Additional faces:

- `rustic-message`
- `rustic-compilation-error`
- `rustic-compilation-warning`
- `rustic-compilation-info`

### rustc errors

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/rustc_errno.png)

## Rustfmt

You can format your code with:

- `rustic-format-buffer` format buffer with stdin
- `rustic-format-file`   format file and revert buffer
- `rustic-cargo-fmt`     run cargo-fmt on workspace
- `rustic-format-region` format active region
- `rustic-format-dwim`   run format on region,file or cargo fmt

Rustic uses the function `rustic-save-some-buffers` for saving buffers
before compilation. In case you prefer using lsp for
formatting, turn off `rustic-format-on-save` and set
`rustic-lsp-format`to `t`.

To save buffers automatically, you can change the value of
`compilation-ask-about-save`, it has higher precedence than
`buffer-save-without-query` when compiling.

```elisp
(defun rustic-mode-auto-save-hook ()
  "Enable auto-saving in rustic-mode buffers."
  (when buffer-file-name
    (setq-local compilation-ask-about-save nil)))
(add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)
```

Customization:

- `rustic-rustfmt-bin` path to rustfmt executable
- `rustic-rustfmt-bin-remote` default path to remote rustfmt executable
- `rustic-rustfmt-args` additional args like +nightly
- `rustic-rustfmt-config-alist` alist of rustfmt configuration options
- `rustic-format-display-method` default function used for displaying
  rustfmt buffer (use the function `ignore`, if you don't want the
  buffer to be displayed)
- `rustic-format-on-save-method` function to use for on-save formatting
- `rustic-format-trigger`
  * `'on-save` format buffer before saving
  * `'on-compile` run 'cargo fmt' before compilation
  * `nil` don't format automatically


known issues:

in case you are using hideshow you might want to set `rustic-format-on-save-method` to `rustic-format-buffer`(#274)

### Change default arguments

If you want to configure the following rustfmt call

```shell
rustfmt +nightly --config hard_tabs=true --config skip_children=false main.rs
```

you can use

```elisp
(setq rustic-rustfmt-args "+nightly")
(setq rustic-rustfmt-config-alist '((hard_tabs . t) (skip_children . nil)))
```

### edition 2018

If you are struggling with errors relating to the Rust edition in
`Cargo.toml`, this may in fact be a problem with `rustfmt` and its
default settings. To solve this, *even though the error message
mentions `Cargo.toml`*, you have to put `edition = "2018"` in a
`rustfmt.toml`. [See here for more
info](https://github.com/rust-lang/rustfmt/issues/4454).

### remote

Currently only `rustic-format-buffer` works remotely.

`rustic-rustfmt-bin` needs to be an absolute path to work remotely.

## LSP

Disable LSP support by setting `rustic-lsp-client` to nil. You have to
restart emacs when you switch lsp clients.

### Server

rust-analyzer is the default and can be changed to rls. lsp-mode
related code was moved to the lsp-mode repo.  `rustic-lsp-server` sets
the value of `lsp-rust-server`.

``` emacs-lisp
(setq rustic-lsp-server 'rls)
```

Change rust-analyzer path.

``` emacs-lisp
(setq rustic-analyzer-command '("~/.cargo/bin/rust-analyzer"))
```

### Client

The default package is `lsp-mode`. But you can also use `eglot`.

``` emacs-lisp
(setq rustic-lsp-client 'eglot)
```

LSP commands:

`xref-find-definitions`

`xref-find-references` with helm and rust-analyzer

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/xref_references.png)

#### eglot

Turn off flymake.

``` emacs-lisp
(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
```

#### lsp-mode

- `lsp-describe-thing-at-point` display documentation
- `lsp-find-definition` makes use of xref

You can find more information in the [lsp-mode documentation for Rust](https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/).

##### `lsp-execute-code-action`

This command can be extremely convenient when applying code actions or
using auto-imports.

Run `lsp-execute-code-action` when lsp-ui displays code actions at the
top of the sideline.

###### Applying code actions

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/code_actions.png)

###### Auto import

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/auto_import.png)

##### Macro expansion

`lsp-rust-analyzer-expand-macro` expand macro call at point
recursively

The results are formatted and highlighted by default, but you can use
your own function by customizing
`lsp-rust-analyzer-macro-expansion-method`.

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/macro_expansion.png)

### LSP + TRAMP

`rust-analyzer` does work over TRAMP, but you have to register the client
manually:

``` elisp
(with-eval-after-load "lsp-rust"
 (lsp-register-client
  (make-lsp-client
   :new-connection (lsp-stdio-connection
                    (lambda ()
                      `(,(or (executable-find
                              (cl-first lsp-rust-analyzer-server-command))
                             (lsp-package-path 'rust-analyzer)
                             "rust-analyzer")
                        ,@(cl-rest lsp-rust-analyzer-server-args))))
   :remote? t
   :major-modes '(rust-mode rustic-mode)
   :initialization-options 'lsp-rust-analyzer--make-init-options
   :notification-handlers (ht<-alist lsp-rust-notification-handlers)
   :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
   :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
   :after-open-fn (lambda ()
                    (when lsp-rust-analyzer-server-display-inlay-hints
                      (lsp-rust-analyzer-inlay-hints-mode)))
   :ignore-messages nil
   :server-id 'rust-analyzer-remote)))
```

*(based on https://github.com/emacs-lsp/lsp-mode/blob/68fddd5d9c5506b2adf6a3f67bbe568f44563dd4/clients/lsp-rust.el#L644)*

If you have Emacs 28, due to some [compatibility issues](https://github.com/emacs-lsp/lsp-mode/issues/2514#issuecomment-759452037),
you might have to additionally use:

``` elisp
(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))

(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)
```

*(thanks to https://github.com/emacs-lsp/lsp-mode/issues/2514#issuecomment-759452037)*

You'll have to have `rust-analyzer` already installed on the target machine.

### Detached file

This is an early experimental feature, and is disabled by default.

Source files not belonging to any crate, or _detached_ source files,
are supported by rust-analyzer, and this feature can be enabled via
`rustic-enable-detached-file-support`. (Currently, only eglot is
supported.)

**Caveat**: Due to some current limitations, you should avoid opening
a detached file in a large directory with this feature enabled.

## Cargo

Since the cargo commands also use the derived compilation mode, you can use
the commands that are mentioned in the "compilation" section.

Customization:

- `rustic-cargo-bin` Path to cargo executable
- `rustic-cargo-bin-remote` Path to remote cargo executable
- `rustic-cargo-build-arguments` default arguments for cargo build
- `rustic-cargo-check-arguments` default arguments for cargo check

### Edit

[cargo-edit](https://github.com/killercup/cargo-edit) provides commands to edit
your dependencies quickly.

The rustic commands can be called with prefix `C-u` if you want to
modify the parameters of a command.

- `rustic-cargo-add`      Add crate to Cargo.toml using 'cargo add'
- `rustic-cargo-rm`       Remove crate from Cargo.toml using 'cargo rm'
- `rustic-cargo-upgrade`  Upgrade dependencies as specified in the local manifest file using 'cargo upgrade'

### Test

If you want to disable warnings when running cargo-test commands, you can set
`(setq rustic-cargo-test-disable-warnings t)`.

Commands:

`rustic-cargo-test` run 'cargo test', when called with `C-u` store
arguments in `rustic-test-arguments`

`rustic-cargo-test-rerun` rerun 'cargo test' with arguments stored in
`rustic-test-arguments`

`rustic-cargo-current-test` run test at point, whether it's a function or a module

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/cargo_current_test.png)

### Run

Based on the usecase, we provide three variants of it:

- `rustic-cargo-run`

This is meant for non interactive programs. It's creates a new mode
which is built on top of `rustic-compilation-mode`. You can press `g`
in this mode's buffer to make it re-run.

- `rustic-cargo-comint-run`

This is meant for both interactive and non interactive programs. For
non interactive programs, you would need to pass data to it via stdin.
It's creates a new mode which is built on top of `comint-mode`. You
can press `C-c C-g` in this mode's buffer to make it re-run.  You can
pass input to the program directly in it's output buffer and press `RET`.

- `rustic-cargo-plain-run`

This is similar to the above `rustic-cargo-comint-run`. Input can be
sent to the program in one of two ways:

- `rustic-compile-send-input`, which reads the input from the
  minibuffer.
- `rustic-cargo-run-use-comint`: when this variable is set to t, the
  input can be typed directly into the output buffer of 'cargo run'
  and sent off with `RET`, just like in `comint-mode`.  You need
  [polymode](https://polymode.github.io) installed for this to work.

### Outdated

Use `rustic-cargo-outdated` to get a list of dependencies that are out
of date. The results are displayed in `tabulated-list-mode` and you
can use most commands you know from the emacs package menu. This
option requires the rust package `cargo-outdated` to be installed
before being used.

- `u` mark single crate for upgrade and prompt user for version.
- `U` mark all upgradable crates.
- `l` mark single crate for upgrading to latest version.
- `L` mark all crates to latest version.
- `m` remove mark
- `x` perform marked package menu actions
- `r` refresh crate list
- `q` quit window

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/outdated.png)

### Expand

[cargo-expand](https://github.com/dtolnay/cargo-expand) provides the ability to expand macros. It also
provides the ability to target a specific modules or a named item
within a module (eg: `module::Type`).

- `rustic-cargo-expand`: runs `cargo expand`. You can also use
  universal argument to target a specific named item to expand.

### More cargo commands

- `rustic-cargo-init` run 'cargo init' to initialize a directory
- `rustic-cargo-new` use 'cargo new' to create a new package
- `rustic-cargo-bench` run 'cargo bench' for the current project
- `rustic-cargo-build-doc` build the documentation for the current project
- `rustic-cargo-doc` open the documentation for the current project in a browser
- `rustic-cargo-lints` called with `rustic-lints-arguments`

## Clippy

Currently cargo does not display the correct installation command for
some toolchains when clippy isn't installed.  If you have problems try
it with `rustup component add --toolchain nightly clippy`.

You can change the parameters `rustic-default-clippy-arguments` that
default to "--benches --tests --all-features".

### auto-fixing before compilation

It's possible to run 'clippy --fix' automatically when starting a compile
process by setting `rustic-cargo-clippy-trigger-fix` to `'on-compile`.
You can also use `'on-save`, but this doesn't work in combination with
automatic formatting.

This feature can be used in combination with auto-formatting.

Works for:

- `rustic-cargo-build`
- `rustic-compile`
- `rustic-recompile`

### Commands

- `rustic-cargo-clippy`     to view the results in a derived compilation mode
- `rustic-cargo-clippy-fix` run 'clippy fix' using `rustic-cargo-clippy-fix-args`
                            the default value is "--allow-dirty"

### Flycheck

In case you want to use clippy with flycheck but without LSP, you can activate
this checker and use the command `flycheck-list-errors`

```elisp
(push 'rustic-clippy flycheck-checkers)
```

Turn off flycheck.

```elisp
(remove-hook 'rustic-mode-hook 'flycheck-mode)
```

The checker automatically detects the active toolchain and applies the
correct parameters. You can set a default value for both stable and
nightly toolchains. These are the default values.

- `rustic-flycheck-clippy-params-stable` "--message-format=json"
- `rustic-flycheck-clippy-params-nightly` "--message-format=json -Zunstable-options"

### lsp-mode

If you are using `lsp-mode` with rust-analyzer, you can set
`lsp-rust-analyzer-cargo-watch-command` to clippy instead of
activating the checker `rustic-clippy`.

## Org-babel

### Intro

Blocks run asynchronously and a running babel process is indicated by
a spinner in the mode-line. It's possible to use crates in babel
blocks. Execute babel block with `org-babel-execute-src-block`.

Supported org babel parameters:

Write to file `:results file :file ~/babel-output`

Customization:

- `rustic-babel-format-src-block` format block after successful build
- `rustic-babel-display-compilation-buffer` display compilation buffer
  of babel process
- `rustic-babel-auto-wrap-main` wrap body into main function

### lsp-mode

You can use lsp in babel blocks with `lsp-org`.

### Commands

- `rustic-babel-format-block` format block at point
- `rustic-babel-visit-project` find generated project of block at point
- `rustic-babel-clippy` run clippy on block(currently doesn't honor babel params, you can open a feature request if you miss it)

### Parameters

#### :crates

This block shows how to use crates with the latest version for both
serde and regex.

The "*" will be added automatically for serde.

```
#+BEGIN_SRC rust :crates '(serde (regex . *))
  extern crate regex;
  extern crate serde;
  use regex::Regex;

  fn main() {
      let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
      assert!(re.is_match("2014-01-01"));
  }
#+END_SRC
```

#### :features

If specific crate features are required then these can be specified
with the `:features` argument. Note that if it is just a single feature
then a string, instead of a list, will also be accepted:

```
#+BEGIN_SRC rust :crates '((tokio . 1.0)) :features '((tokio . ("rt-multi-thread" "time")))
  extern crate tokio;

  fn main() {
      tokio::runtime::Runtime::new()
          .unwrap()
          .block_on(async {
              tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
          });
  }
#+END_SRC
```

#### :paths

Similarly, to depend on local Rust crates, you can set the `:paths`
argument:

```
#+BEGIN_SRC rust :crates '((foo . 1.0)) :paths '((foo . "/home/you/code/foo"))
  use foo::Foo;

  fn main() {
    // Your code.
  }
#+END_SRC
```

#### :toolchain

You can also specify the `:toolchain`. Remember to quote the value!

```
#+begin_src rust :toolchain 'nightly
fn main() {
    let foo: String = vec!["a", "b", "c"].into_iter().intersperse(",").collect();

    println!("{}", foo);
}
#+end_src

#+RESULTS:
: a,b,c
```

#### :main

Auto wrap whole block body in a `fn main` function call if none
exists.

Since this is very handy in most code snippets, so the default value
is `yes`.  `no` if you don't want this feature(for example, you don't
want regex search slow things down).

You can also set a default value by:
``` elisp
;; By setq this default to `nil`, you'll have to explict set params to ":main yes" in each block
(setq rustic-babel-auto-wrap-main nil)
```

```
#+begin_src rust :main yes
let x = vec![1, 2, 3].iter().map(|&x| x + 1).collect::<Vec<_>>();
println!("{:?}", x);
#+end_src

#+results:
: [2, 3, 4]
```

#### :include

This parameter allows you to run code that is located in different
babel blocks by using named blocks with the `:include` keyword. This
feature only concats the blocks so you don't need to import the code
you want to use.

You can still use `:main` to wrap the code of the main block.

```
#+name: b1
#+begin_src rust
pub fn b1_func() -> String {
    String::from("b1 function called")
}
#+end_src

#+name: b2
#+begin_src rust
pub fn b2_func() -> String {
    String::from("b2 function called")
}
#+end_src

#+begin_src rust :include '("b1" "b2")
  fn main() {
      println!("{:?}", b1_func());
      println!("{:?}", b2_func());
  }
#+end_src

#+RESULTS:
: "b1 function called"
: "b2 function called"
```

#### :use

When using this keyword blocks are treated as modules. The files are
generated automatically.

```
#+name: mymodule
#+begin_src rust
pub fn myfunc() -> String {
    String::from("mymodule function called")
}
#+end_src

#+begin_src rust :use '("mymodule")
use mymodule::myfunc;

fn main() {
    println!("{:?}", myfunc());
}
#+end_src

#+RESULTS:
: "mymodule function called"
```

## Spinner

In case you want to use a different spinner type you can modify
`rustic-spinner-type` or turn it off completely with
`rustic-display-spinner`.([Available spinner
types](https://github.com/Malabarba/spinner.el/blob/master/spinner.el#L104)).

## rust docs in org-mode

It is possible to read rust documentation inside Emacs! This currently
requires LSP-mode and cargo.  ![Rustic-doc
example](img/rustic-doc.png)

### Prerequisites

Required:

- [pandoc](https://pandoc.org/installing.html) preferably at least version 2.11, as it will give somewhat nicer generated documentation. Versions older than 2.9 may not work - if you're on a debian based distro installing through your regular repo might not work out.
- [cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
- [cargo-makedocs](https://github.com/Bunogi/cargo-makedocs)
- [fd-find](https://github.com/sharkdp/fd) Old versions, especially before 2.x, may not work. Install through Cargo if you're having issues.

Optional:

- [helm-ag](https://github.com/emacsorphanage/helm-ag)
- [ripgrep](https://github.com/BurntSushi/ripgrep)

`ripgrep` and `helm-ag` are optional but highly recommended.

If only ripgrep is installed, it will be used with the emacs `grep`
command.  In case neither is available, the emacs `grep` command will
use `grep`, like in the good old days.

When a required cargo package is missing you will be asked if you want
to install them when running rustic-doc-setup.

### Usage

- Enable `rustic-doc-mode`.
- Run `rustic-doc-setup` to download files that rustic-doc needs to
  convert rust documentation and also convert `std`.
- You can now convert package-specific documentation with
  `rustic-doc-convert-current-package`
- Search the org files with `rustic-doc-search` (bound to `C-#` by
  default) if you are in `Rust mode`, `Rustic mode` or `Org mode`. If
  you hover over a symbol when you invoke the command,
  `rustic-doc-search` will insert a default value.
- Add `universal argument` to only search for level 1 headers like
  struct or enum names.

You can change the defaults by modifying

- `rustic-doc-rg-search-command`
- `rustic-doc-search-function`

### Notes

- You should re-run `rustic-doc-setup` once in a while, to update the
  pandoc filter.
- If rustic-doc does not find the documentation for something, the
  first thing to do is check the project's `target/doc` folder for the
  corresponding `.html-file`. If there is no file there, there is
  nothing for rustic-doc to convert. If there is a file there, please
  create an issue!

## Popup

You can execute commands with `rustic-popup`(call it with optional
argument `C-u` to choose a directory). The list of commands can be
customized with `rustic-popup-commands`.  The command
`rustic-popup-default-action` (`RET` or `TAB`) allows you to change:

- `RUST_BACKTRACE` environment variable
- `compilation-arguments` for `recompile`
- arguments for `cargo test`

If you want to close the popup after you ran a command you can set
`rustic-kill-buffer-and-window` to `t`.

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/popup.png)

View help buffer containing a command's flags with `h`:

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/popup_help.png)

## rust-mode

rustic-mode derives from rust-mode, however we replace default key
bindings and some hooks.

There are also some additional commands:

- `rust-dbg-wrap-or-unwrap` Either remove or add the dbg! macro
- `rust-toggle-mutability` Toggles the mutability of the variable defined on the current line
- `rust-promote-module-into-dir` Promote the module file visited by the current buffer into its own directory

## elisp tests

To run the tests, you will need [Cask](https://github.com/cask/cask).

``` bash
cask exec ert-runner
```

alternatively you can use `make test`

## Contributing

PRs, feature requests and bug reports are very welcome. If you want to
add a new feature please open an issue in advance so we can discuss
the details.
