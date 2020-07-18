# Rustic

[![MELPA](https://melpa.org/packages/rustic-badge.svg)](https://melpa.org/#/rustic)
[![Build
Status](https://travis-ci.com/brotzeit/rustic.svg?branch=master)](https://travis-ci.com/brotzeit/rustic)
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Rustic](#rustic)
    - [Intro](#intro)
    - [Installation](#installation)
        - [straight](#straight)
    - [Compilation](#compilation)
        - [Faces](#faces)
        - [rustc errors](#rustc-errors)
    - [Rustfmt](#rustfmt)
        - [edition 2018](#edition-2018)
    - [LSP](#lsp)
        - [Server](#server)
        - [Client](#client)
            - [eglot](#eglot)
            - [lsp-mode](#lsp-mode)
                - [`lsp-execute-code-action`](#lsp-execute-code-action)
                    - [Applying code actions](#applying-code-actions)
                    - [Auto import](#auto-import)
                - [Macro expansion](#macro-expansion)
    - [Cargo](#cargo)
        - [Test](#test)
        - [Outdated](#outdated)
    - [Clippy](#clippy)
        - [Flycheck](#flycheck)
        - [lsp-mode](#lsp-mode-1)
    - [Org-babel](#org-babel)
    - [Popup](#popup)
    - [elisp tests](#elisp-tests)
    - [Contributing](#contributing)

<!-- markdown-toc end -->


## Intro

This package is a fork of
[rust-mode](https://github.com/rust-lang/rust-mode)

Differences with rust-mode:

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
- cask for testing
- requires emacs 26
- etc.

[Why fork](https://github.com/brotzeit/rustic/issues/92) ?

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

## Compilation

Rustic defines a derived compilation-mode. Colors can be customized
with several defcustoms.  You can use `next-error` and
`compilation-next-error` as for any other compilation buffer.

However it's possible to also jump to line numbers that are displayed
at the beginning of a line.  This feature is provided by a hook around
`compile-goto-error`(`RET`).

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/compilation_buffer.png)

Customization:

- `rustic-compile-display-method` choose function that displays the
  compilation buffer
- `rustic-compile-backtrace` change backtrace verbosity
- `rustic-compile-command` default command for rust compilation

Supported compile.el variables:

- compilation-arguments
- compilation-scroll-output

### Faces

The colors that are displayed in compilation buffers come from cargo
and are translated by xterm-color. You can change these colors by
modifying `rustic-ansi-faces`.

`rustic-compilation-mode` doesn't use the default faces of
compile.el. If you want to change these colors you can use something
similiar to:

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

You can format your code with `rustic-format-buffer` or
`rustic-cargo-fmt`. Rustic uses the function
`rustic-save-some-buffers` for saving buffers before compilation. To
save buffers automatically, you can change the value of
`buffer-save-without-query`. In case you prefer using lsp for
formatting, turn off `rustic-format-on-save` and set
`rustic-lsp-format`to `t`.

Commands:
- `rustic-compile-send-input` send string to process of current buffer

Customization:

- `rustic-rustfmt-bin` path to rustfmt executable
- `rustic-rustfmt-config-alist` alist of rustfmt configuration options
- `rustic-format-display-method` default function used for displaying
  rustfmt buffer (use the function `ignore`, if you don't want the
  buffer to be displayed)
- `rustic-format-trigger`
  * `'on-save` format buffer before saving
  * `'on-compile` run 'cargo fmt' before compilation
  * `nil` don't format automatically

### edition 2018

You have to put `edition = "2018"` in a `rustfmt.toml`.

## LSP

Disable LSP support by setting `rustic-lsp-client` to nil. You have to
restart emacs when you switch lsp clients.

### Server

RLS is the default and can be changed to rust-analyzer. lsp-mode
related code was moved to the lsp-mode repo.  `rustic-lsp-server` sets
the value of `lsp-rust-server`.

``` emacs-lisp
(setq rustic-lsp-server 'rust-analyzer)
```

Change rust-analyzer path.

``` emacs-lisp
(setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
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
wiki](https://github.com/emacs-lsp/lsp-mode/wiki/Rust).

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

## Cargo

### Test

`rustic-cargo-test` run 'cargo test', when called with `C-u` store
arguments in `rustic-test-arguments`

`rustic-cargo-test-rerun` rerun 'cargo test' with arguments stored in
`rustic-test-arguments`

`rustic-cargo-current-test` run test at point

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/cargo_current_test.png)

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

## Clippy

Currently cargo does not display the correct installation command for
some toolchains when clippy isn't installed.  If you have problems try
it with `rustup component add --toolchain nightly clippy`.

Use `rustic-cargo-clippy` to view the results in a derived compilation
mode.

### Flycheck

In case you want to see clippy lints with flycheck, you can activate
this checker and use the command `flycheck-list-errors`

```elisp
(push 'rustic-clippy flycheck-checkers)
```

Turn off flycheck.

```elisp
(remove-hook 'rustic-mode-hook 'flycheck-mode)
```

The parameters of the checker can be modified with `rustic-flycheck-clippy-params`
and are by default configured for using unstable options that are only available
on the nightly toolchains.

If you are using the stable toolchain you have to change the value:

```elisp
(setq rustic-flycheck-clippy-params "--message-format=json")
```

### lsp-mode

If you are using `lsp-mode` with rust-analyzer, you can set
`lsp-rust-analyzer-cargo-watch-command` to clippy instead of
activating the checker `rustic-clippy`.

## Org-babel

Blocks run asynchronously and a running babel process is indicated by
a spinner in the mode-line. It's possible to use crates in babel
blocks.

Execute babel block with `org-babel-execute-src-block`

```
#+BEGIN_SRC rust :crates '((regex . 0.2))
  extern crate regex;

  use regex::Regex;

  fn main() {
      let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
      assert!(re.is_match("2014-01-01"));
  }
#+END_SRC
```

Supported org babel parameters:

Write to file `:results file :file ~/babel-output`

Customization:

- `rustic-babel-format-src-block` format block after successful build
- `rustic-babel-display-compilation-buffer` display compilation buffer
  of babel process
- `rustic-display-spinner` turn off spinner in the mode-line

## Popup

You can execute commands with `rustic-popup`. The list of commands can
be customized with `rustic-popup-commands`. It's also possible to view
the command's flags with `h`.  The command
`rustic-popup-default-action` (`RET` or `TAB`) allows you to change:

- `RUST_BACKTRACE` environment variable
- `compilation-arguments` for `recompile`
- arguments for `cargo test`

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/popup.png)

## elisp tests

To run the tests, you will need [Cask](https://github.com/cask/cask).

``` bash
make test
```

## Contributing

PRs, feature requests and bug reports are very welcome.
