## Shortcuts for rust-mode:

* `C-c C-c C-u` rust-compile
* `C-c C-c C-i` rust-recompile
* `C-c C-c C-b` rust-cargo-build
* `C-c C-c C-f` rust-cargo-fmt
* `C-c C-c C-t` rust-cargo-test
* `C-c C-c C-l` rust-cargo-clippy
* `C-c C-c C-o` rust-format-buffer

## Org-babel

```
#+BEGIN_SRC rust :crates '(("regex" . "0.2") ("darling" . "0.1"))
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

## Installation

git clone https://github.com/brotzeit/rust-mode-reloaded.git

You can use `package-install-file` on the package's root directory
