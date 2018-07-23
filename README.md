[![MELPA](https://melpa.org/packages/rustic-badge.svg)](https://melpa.org/#/rustic)

This package is a fork of [rust-mode](https://github.com/rust-lang/rust-mode)

Differences with rust-mode:

- translation of ANSI control sequences through [xterm-color](https://github.com/atomontage/xterm-color)
- async org babel
- custom compilation process
- rustfmt errors in a rust compilation mode
- cask for testing
- etc.

## Some Commands:

* rustic-compile
* rustic-recompile
* rustic-cargo-build
* rustic-cargo-fmt
* rustic-cargo-test
* rustic-cargo-clippy
* rustic-cargo-list-outdated
* rustic-format-buffer
* rustic-racer-describe

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
(require 'eglot)
;; replace rust-mode with rustic
(setq eglot-server-programs
      `((rustic-mode . (eglot-rls "rls"))
        ,@(-remove-first  (lambda (mode)
                            (string= (car mode) 'rust-mode))
                          eglot-server-programs)))

;; start rls server automatically
(add-hook 'rustic-mode-hook 'eglot-ensure)

;; don't allow formatting with rls
(add-to-list 'eglot-ignored-server-capabilites :documentFormattingProvider)
```

