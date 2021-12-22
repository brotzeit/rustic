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
        - [default directory](#default-directory)
        - [Faces](#faces)
        - [rustc errors](#rustc-errors)
    - [Rustfmt](#rustfmt)
        - [Change default arguments](#change-default-arguments)
        - [edition 2018](#edition-2018)
        - [remote](#remote)
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
    - [Cargo](#cargo)
        - [Edit](#edit)
        - [Test](#test)
        - [Run](#run)
        - [Outdated](#outdated)
        - [More cargo commands](#more-cargo-commands)
    - [Clippy](#clippy)
        - [Commands](#commands)
        - [Flycheck](#flycheck)
        - [lsp-mode](#lsp-mode-1)
    - [Org-babel](#org-babel)
        - [Intro](#intro-1)
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
    - [rust docs in org mode](#rust-docs-in-org-mode)
        - [Prequisites](#prequisites)
        - [Usage](#usage)
        - [Notes](#notes)
    - [Popup](#popup)
    - [rust-mode](#rust-mode)
    - [elisp tests](#elisp-tests)
    - [Contributing](#contributing)

<!-- markdown-toc end -->


## Intro

This package is based on [rust-mode](https://github.com/rust-lang/rust-mode) and provides additional features:

- flycheck integration
- cargo popup
- multiline error parsing
- translation of ANSI control sequences through
  [xterm-color](https://github.com/atomontage/xterm-color)
- async org babel
- custom compilation process
- rustfmt errors in a rust compilation mode
- automatic LSP configuration with
  [eglot](https://github.com/joaotavora/eglot) or
  [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
- Optional rust inline documentation
- cask for testing
- requires emacs 26
- etc.

rustic only shares the rust-mode code from rust-mode.el and rust-utils.el.
The other files provide functionality that is similar to some of the features
of rustic, however can be considered light-weight compared to some rustic's
functionality.

The shared functions and options exist as aliases in the rust-mode and
rustic namespace for backwards compatability reasons(rustic has been a fork).

## Known issues

- `rustic-format-buffer` occasionally sets point to beginning of buffer,
   use `rustic-format-file` instead
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

If you have `rust-mode` installed, ensure it is required before rustic
since it has to be removed from `auto-mode-alist`. However you only
need `rust-mode` if you want to use `emacs-racer`. There's some stuff
that isn't included in rustic.

If you have any issues with rustic, please try running emacs without
`rust-mode` loaded.

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

Rustic defines a derived compilation-mode. Colors can be customized
with several defcustoms.  You can use `next-error` and
`compilation-next-error` as for any other compilation buffer.

However it's possible to also jump to line numbers that are displayed
at the beginning of a line.  This feature is provided by a hook around
`compile-goto-error`(`RET`).

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
- `rustic-compile-command` default command for rust compilation
- `rustic-compile-command-remote` default command for remote rust compilation

Supported compile.el variables:

- compilation-arguments
- compilation-scroll-output

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

Rustic uses the function `rustic-save-some-buffers` for saving buffers
before compilation. To save buffers automatically, you can change the
value of `buffer-save-without-query`. In case you prefer using lsp for
formatting, turn off `rustic-format-on-save` and set
`rustic-lsp-format`to `t`.

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

You can find more information in the [lsp-mode
wiki](https://emacs-lsp.github.io/lsp-mode/page/lsp-rust/).

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

## Cargo

Customization:

- `rustic-cargo-bin` Path to cargo executable
- `rustic-cargo-bin-remote` Path to remote cargo executable

### Edit

[cargo-edit](https://github.com/killercup/cargo-edit) provides commands to edit
your dependencies quickly.

The rustic commands can be called with prefix `C-u` if you want to
modify the parameters of a command.

- `rustic-cargo-add`      Add crate to Cargo.toml using 'cargo add'
- `rustic-cargo-rm`       Remove crate from Cargo.toml using 'cargo rm'
- `rustic-cargo-upgrade`  Upgrade dependencies as specified in the local manifest file using 'cargo upgrade'

### Test

`rustic-cargo-test` run 'cargo test', when called with `C-u` store
arguments in `rustic-test-arguments`

`rustic-cargo-test-rerun` rerun 'cargo test' with arguments stored in
`rustic-test-arguments`

`rustic-cargo-current-test` run test at point, whether it's a function or a module

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/cargo_current_test.png)

### Run

`rustic-cargo-run` run 'cargo run'.  Input can be sent to the program
in one of two ways:

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

- `u` mark single crate for upgrade
- `U` mark all upgradable crates
- `m` remove mark
- `x` perform marked package menu actions
- `r` refresh crate list
- `q` quit window

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/outdated.png)

### More cargo commands

- `rustic-cargo-init` run 'cargo init' to initialize a directory
- `rustic-cargo-new` use 'cargo new' to create a new package
- `rustic-cargo-bench` run 'cargo bench' for the current project
- `rustic-cargo-build-doc` build the documentation for the current project
- `rustic-cargo-doc` open the documentation for the current project in a browser

## Clippy

Currently cargo does not display the correct installation command for
some toolchains when clippy isn't installed.  If you have problems try
it with `rustup component add --toolchain nightly clippy`.

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
correct parameters You can set a default value for both stable and
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

- [pandoc](https://pandoc.org/installing.html) (preferably at least version 2.11, as it will give somewhat nicer generated documentation)
- [cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
- [cargo-makedocs](https://github.com/Bunogi/cargo-makedocs)
- [fd-find](https://github.com/sharkdp/fd)

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

You can execute commands with `rustic-popup`. The list of commands can
be customized with `rustic-popup-commands`.  The command
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

The rustic mode derives from rust-mode using all of its functionality. However
we replace default key bindings and some hooks.

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
