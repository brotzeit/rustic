# Rustdoc to org
A Pandoc filter that converts rust documentation to .org-files, and a minor mode to go with!
![Demo with helm ag](demo.gif)

## Installation

Installation is somewhat annoying at the moment. However, most of these should not be too painful to setup.

### Prequisites

* Install Pandoc https://pandoc.org/installing.html
* Install cargo https://doc.rust-lang.org/cargo/getting-started/installation.html
* Install ripgrep with `cargo install ripgrep` or one of the alternatives: https://github.com/BurntSushi/ripgrep#installation
* Install cargo-makedocs by running `cargo install cargo-makedocs` https://github.com/Bunogi/cargo-makedocs
* Install fd with `cargo install fd`, or your package manager

### The package

Rustdoc is not on melpa, but you can install it from here with your package manager of choice or manually. Please make an issue if something stops working, as rustdoc is currently changing rapidly.
* With [straight.el](https://github.com/raxod502/straight.el#the-recipe-format)
* With quelpa: `(quelpa '(rustdoc :fetcher github :repo "samhedin/rustdoc-to-org"))`
* Manually
    * Install helm-ag from MELPA with <kbd>M-x package-install [RET] helm-ag [RET]</kbd> https://github.com/bridgesense/emacs-helm-ag#installation
    * Copy `rustdoc.el` and load it with `(require rustdoc.el)`

## Usage

* Enable `rustdoc-mode`.
* Run `M-x rustdoc-setup` to download files that rustdoc needs to convert rust documentation and also convert `std`.
* You can now convert package-specific documentation with `M-x rustdoc-convert-current-package`
* Search the org files with `rustdoc-search` (bound to `C-#` by default) if you are in `Rust mode`, `Rustic mode` or `Org mode`. If you hover over a symbol when you invoke the command, `rustdoc-search` will insert a default value.
* Add `universal argument` to only search for level 1 headers like struct or enum names.

## Notes
* We are waiting for an update to Pandoc that will make the generated documents prettier, it should be available soon https://github.com/jgm/pandoc/issues/6554
* You should re-run `rustdoc-setup` once in a while, to update the pandoc filter.
* If rustdoc does not find the documentation for something, the first thing to do is check the project's `target/doc` folder for the corresponding `.html-file`. If there is no file there, there is nothing for rustdoc to convert. If there is a file there, please create an issue!

## TODO

* Figure out a way to get links working. All links point to .html files, they should be updated to point to .org files.
* Make code not awful and not slow.
* Fill out this list.

All suggestions, comments, bug reports, PRs etc are very welcomed!
