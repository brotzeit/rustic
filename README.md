This package is a fork of [rust-mode](https://github.com/rust-lang/rust-mode)

Differences with rust-mode:

- translation of ANSI control sequences through [xterm-color](https://github.com/atomontage/xterm-color)
- async org babel
- custom compilation process
- rustfmt errors in compilation mode
- cask for testing

## Shortcuts:

* `C-c C-c C-u` rustic-compile
* `C-c C-c C-i` rustic-recompile
* `C-c C-c C-b` rustic-cargo-build
* `C-c C-c C-r` rustic-cargo-run
* `C-c C-c C-f` rustic-cargo-fmt
* `C-c C-c C-t` rustic-cargo-test
* `C-c C-c C-l` rustic-cargo-clippy
* `C-c C-c C-o` rustic-format-buffer
* `C-c C-c C-n` rustic-cargo-list-outdated

## Org-babel

```
#+BEGIN_SRC rustic :crates '(("regex" . "0.2") ("darling" . "0.1"))
fn main() {
    println!("{}", "foo");
}
#+END_SRC
```

## Testing

To run the tests, you will
need [Cask](https://github.com/cask/cask).

``` bash
$ cask
$ cask exec ert-runner
```

## RLS through eglot

``` emacs-lisp
(setq eglot-server-programs '((rustic-mode . (eglot-rls "rls"))))
```

## Installation

git clone https://github.com/brotzeit/rust-mode-reloaded.git

You can use `package-install-file` on the package's root directory

