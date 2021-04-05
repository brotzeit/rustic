## Common

.PHONY: test

-include config.mk

all: lisp

PKG = rustic

EMACS ?= emacs
EMACS_ARGS ?=

ELS   = rustic-common.el
ELS  += rustic.el
ELS  += rustic-util.el
ELS  += rustic-compile.el
ELS  += rustic-popup.el
ELS  += rustic-cargo.el
ELS  += rustic-babel.el
ELS  += rustic-racer.el
ELS  += rustic-interaction.el
ELS  += rustic-flycheck.el
ELCS  = $(ELS:.el=.elc)

## Without Cask
ifdef WITHOUT_CASK

DEPS  = company
DEPS += dash
DEPS += f
DEPS += flycheck
DEPS += eglot
DEPS += helm-ag
DEPS += ht
DEPS += hydra
DEPS += lsp-mode
DEPS += lsp-mode/clients
DEPS += lv
DEPS += markdown-mode
DEPS += org/lisp
DEPS += projectile
DEPS += s
DEPS += spinner
DEPS += xterm-color
DEPS += yasnippet

LOAD_PATH  ?= $(addprefix -L ../,$(DEPS))
LOAD_PATH  += -L .

TEST_ELS  = test/test-helper.el
TEST_ELS += $(wildcard test/rustic-*-test.el)

lisp: $(ELCS) loaddefs

%.elc: %.el
	@printf "Compiling $<\n"
	@$(EMACS) -Q --batch $(EMACS_ARGS) \
	$(LOAD_PATH) --funcall batch-byte-compile $<

test:
	@$(EMACS) -Q --batch $(EMACS_ARGS) $(LOAD_PATH) -L test \
	--load $(CURDIR)/test/test-helper \
	$(addprefix -l ,$(TEST_ELS)) \
	--funcall ert-run-tests-batch-and-exit

## With Cask
else

CASK_DIR := $(shell EMACS=$(EMACS) cask package-directory)

$(CASK_DIR): Cask
	EMACS=$(EMACS) cask install
	touch $(CASK_DIR)

cask-install: $(CASK_DIR)

cask-build: loaddefs
	EMACS=$(EMACS) cask build

lisp: cask-build

test: lisp
	if [ -f "$(HOME)/.cargo/env" ] ; then . "$(HOME)/.cargo/env" ; fi ; \
	EMACS=$(EMACS) cask exec ert-runners

## Common
endif

CLEAN  = $(ELCS) $(PKG)-autoloads.el

clean:
	@printf "Cleaning...\n"
	@rm -rf $(CLEAN)

loaddefs: $(PKG)-autoloads.el

define LOADDEFS_TMPL
;;; $(PKG)-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name \
(or (file-name-directory #$$) (car load-path))))

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; $(PKG)-autoloads.el ends here
endef
export LOADDEFS_TMPL
#'

$(PKG)-autoloads.el: $(ELS)
	@printf "Generating $@\n"
	@printf "%s" "$$LOADDEFS_TMPL" > $@
	@$(EMACS) -Q --batch --eval "(progn\
	(setq make-backup-files nil)\
	(setq vc-handled-backends nil)\
	(setq default-directory (file-truename default-directory))\
	(setq generated-autoload-file (expand-file-name \"$@\"))\
	(setq find-file-visit-truename t)\
	(update-directory-autoloads default-directory))"
