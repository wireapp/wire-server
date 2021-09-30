LANG := en_US.UTF-8
SHELL := /usr/bin/env bash
NAME := billing-team-member-backfill
EXECUTABLES_COMPONENTS := wire-server:billing-team-member-backfill

default: all

all: install

.PHONY: clean
clean:
	stack clean $NAME

.PHONY: compile
compile:
	stack build $(EXECUTABLES_COMPONENTS)

.PHONY: install
install:
	stack install --pedantic --local-bin-path=dist $(EXECUTABLES_COMPONENTS)

.PHONY: fast
fast:
	stack install --fast --local-bin-path=dist $(EXECUTABLES_COMPONENTS)
