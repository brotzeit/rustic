# Rustic

[![MELPA](https://melpa.org/packages/rustic-badge.svg)](https://melpa.org/#/rustic)
[![Build Status](https://travis-ci.com/brotzeit/rustic.svg?branch=master)](https://travis-ci.com/brotzeit/rustic)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Intro](#intro)
- [Installation](#installation)
- [Compilation](#compilation)
- [Rustfmt](#rustfmt)
- [LSP](#lsp)
    - [Server](#server)
    - [Client](#client)
- [Rustfix](#rustfix)
- [Clippy](#clippy)
- [Flycheck](#flycheck)
- [Org-babel](#org-babel)
- [Popup](#popup)
- [Cargo outdated](#cargo-outdated)
- [Tests](#tests)
- [Contributing](#contributing)

<!-- markdown-toc end -->

# Intro

This package is a fork of [rust-mode](https://github.com/rust-lang/rust-mode)

Differences with rust-mode:

- [rust-analyzer](https://github.com/rust-analyzer/rust-analyzer) configuration
- flycheck integration
- cargo popup
- multiline error parsing
- translation of ANSI control sequences through [xterm-color](https://github.com/atomontage/xterm-color)
- async org babel
- custom compilation process
- rustfmt errors in a rust compilation mode
- automatic RLS configuration with [eglot](https://github.com/joaotavora/eglot) or [lsp-mode](https://github.com/emacs-lsp/lsp-mode)
- cask for testing
- requires emacs 26
- etc.

# Installation

Simply put `(use-package rustic)` in your config and most stuff gets configured automatically.
([use-package](https://github.com/jwiegley/use-package))

It's necessary to include elpa for a package dependency:

```elisp
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
```

If ‘spinner-1.7.3’ is unavailable” when trying to install rustic, you need to update GPG keys
used by the ELPA package manager. Try installing [gnu-elpa-keyring-update](https://elpa.gnu.org/packages/gnu-elpa-keyring-update.html).

If you have `rust-mode` installed, ensure it is required before rustic since it has to be removed
from `auto-mode-alist`. However you only need `rust-mode` if you want to use `emacs-racer`. There's some stuff that isn't included in rustic.

Note: If you have any issues with rustic, please try running emacs without `rust-mode` loaded.

# Compilation

Rustic defines a derived compilation-mode. Colors can be customized with several defcustoms.

Customization:

- `rustic-compile-display-method` choose function that displays the compilation buffer
- `rustic-compile-backtrace`      change backtrace verbosity
- `rustic-compile-command`        default command for rust compilation

Supported compile.el variables:

- compilation-arguments
- compilation-scroll-output (not `first-error`)

# Rustfmt

You can format your code with `rustic-format-buffer` or `rustic-cargo-fmt`. Rustic uses the function `rustic-save-some-buffers` for saving buffers before compilation. To save buffers automatically, you can change the value of `buffer-save-without-query`. In case you prefer using lsp for formatting, turn off `rustic-format-on-save` and set `rustic-lsp-format`to `t`.

Rust edition 2018 requires a `rustfmt.toml` file.

Customization:

- `rustic-rustfmt-bin`           path to rustfmt executable
- `rustic-format-display-method` default function used for displaying rustfmt buffer (use the function `ignore`, if you don't want the buffer to be displayed)
- `rustic-format-trigger` 
  * `'on-save`    format buffer before saving
  * `'on-compile` run 'cargo fmt' before compilation
  * `nil`         don't format automatically

# LSP

Disable LSP support by setting `rustic-lsp-client` to nil. You have to restart emacs when you switch lsp clients.

LSP commands:

`xref-find-definitions` find definition

## Server

RLS is the default and can be changed to rust-analyzer. lsp-mode related code was moved to the lsp-mode repo.
`rustic-lsp-server` sets the value of `lsp-rust-server`.

``` emacs-lisp
(setq rustic-lsp-server 'rust-analyzer)
```

## Client

The default package is `lsp-mode`. But you can also use `eglot`.

``` emacs-lisp
(setq rustic-lsp-client 'eglot)
```

# Rustfix

`rustic-rustfix` runs rustfix, but it seems it's not very mature at this point.
It would be a lot more helpful if we had an indicator for fixable errors and if it would be
possible to run rustfix on a single file.

# Clippy

Rustic automatically configures a checker that runs clippy when `flycheck` is required.
In case you use `flymake`, you have to take care of the configuration yourself.

Currently cargo does not display the correct installation command for some toolchains when
clippy isn't installed. 
If you have problems try it with `rustup component add --toolchain nightly clippy`.

Use `rustic-cargo-clippy` to view the results in a derived compilation mode.

# Flycheck

By default, rustic displays the number of errors and warnings in the mode-line. The variable
`rustic-flycheck-setup-mode-line-p` can be set to `nil` to turn off mode-line modification.

# Org-babel

Blocks run asynchronously and a running babel process is indicated by a spinner
in the mode-line. It's possible to use crates in babel blocks.

```
#+BEGIN_SRC rustic :crates '((regex . 0.2))
  extern crate regex;

  use regex::Regex;

  fn main() {
      let re = Regex::new(r"^\d{4}-\d{2}-\d{2}$").unwrap();
      assert!(re.is_match("2014-01-01"));
  }
#+END_SRC
```

Supported org babel parameters:

Write to file `:results file :file ~/babel-file`

Customization:

- `rustic-babel-format-src-block` format block after successful build
- `rustic-babel-display-compilation-buffer` display compilation buffer of babel process
- `rustic-display-spinner` turn off spinner in the mode-line

# Popup

You can execute commands with `rustic-popup`. The list of commands can be customized
with `rustic-popup-commands`. It's also possible to view the command's flags with `h`.
The command `rustic-popup-default-action` (`RET` or `TAB`) allows you to change:

- `RUST_BACKTRACE` environment variable
- `compilation-arguments` for `recompile`
- arguments for `cargo test`

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/popup.png)

# Cargo outdated

Use `rustic-cargo-outdated` to get a list of dependencies that are out of date. The results 
are displayed in `tabulated-list-mode` and you can use most commands you know from the emacs
package menu. This option requires the rust package `cargo-outdated` to be
installed before being used.

- `u` mark single crate for upgrade
- `U` mark all upgradable crates
- `m` remove mark
- `x` perform marked package menu actions
- `r` refresh crate list
- `q` quit window

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/outdated.png)

# Tests

To run the tests, you will need [Cask](https://github.com/cask/cask).

``` bash
$ cask
$ cask exec ert-runner
```

# Contributing

PRs, feature requests and bug reports are very welcome.
