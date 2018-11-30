[![MELPA](https://melpa.org/packages/rustic-badge.svg)](https://melpa.org/#/rustic)
[![Build Status](https://travis-ci.com/brotzeit/rustic.svg?branch=master)](https://travis-ci.com/brotzeit/rustic)

# Rustic

This package is a fork of [rust-mode](https://github.com/rust-lang/rust-mode)

Differences with rust-mode:

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

## Org-babel

```
#+BEGIN_SRC rustic :crates '(("regex" . "0.2") ("darling" . "0.1"))
fn main() {
    println!("{}", "foo");
}
#+END_SRC
```

## Rust Language Server

The default package is `lsp-mode`. But you can also use `eglot` or no RLS client with `nil`.

``` emacs-lisp
(setq rustic-rls-pkg 'eglot)
```

## Popup

You can execute commands with `rustic-popup`. The list of commands can be customized
with `rustic-popup-commands`. You can open a buffer that shows the command's flags with `h`.
The command `rustic-popup-default-action` (`RET` or `<tab>`) allows you to change:

- BACKTRACE variable `rustic-compile-backtrace`
- `compilation-arguments` for `recompile`
- arguments for `cargo test`

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/popup.png)

## Cargo outdated

Use `rustic-cargo-outdated` to get a list of dependencies that are out of date. The results 
are displayed in `tabulated-list-mode` and you can use most commands you know from the emacs
package menu.

![](https://raw.githubusercontent.com/brotzeit/rustic/master/img/outdated.png)

## Contributing

PRs, feature requests and bug reports are very welcome.
