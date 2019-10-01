EMACS ?= emacs
SHELL := /bin/bash

all:
	EMACS=$(EMACS) cask install
	EMACS=$(EMACS) cask build
	EMACS=$(EMACS) cask clean-elc

test: all
	if [ -f "$(HOME)/.cargo/env" ] ; then source "$(HOME)/.cargo/env" ; fi ; \
	EMACS=$(EMACS) cask exec ert-runner

.PHONY: all test
