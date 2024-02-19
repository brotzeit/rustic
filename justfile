# List all recipes
just:
	just --list --unsorted

# Install dependencies and build via cask
build:
	emacs --version
	cask install
	cask build

# Test
test:
	cask emacs --batch -L . -L test -f batch-byte-compile $(cask files)
	cask emacs --batch -L . -L test -l test/all-tests.el -f ert-run-tests-batch-and-exit
