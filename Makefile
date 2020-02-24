SHELL := /bin/bash
export CASK := $(shell which cask)
ifeq ($(CASK),)
$(error Please install CASK at https://cask.readthedocs.io/en/latest/guide/installation.html)
endif
CASK_DIR := $(shell $(CASK) package-directory || exit 1)

all: cask
	$(CASK) build

cask: $(CASK_DIR)

clean:
	$(CASK) clean-elc

$(CASK_DIR): Cask
	$(CASK) install
	touch $(CASK_DIR)

test: cask
	if [ -f "$(HOME)/.cargo/env" ] ; then source "$(HOME)/.cargo/env" ; fi ; \
	$(CASK) exec ert-runner

.PHONY: all test cask clean
