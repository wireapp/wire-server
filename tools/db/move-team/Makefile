LANG := en_US.UTF-8

SHELL := /usr/bin/env bash

default: all

all: fast

.PHONY: init
init:
	mkdir -p dist

.PHONY: clean
clean:
	stack clean

.PHONY: compile
compile:
	stack build .

.PHONY: install
install: init
	stack install --pedantic --local-bin-path=dist .

.PHONY: fast
fast: init
	stack install --fast --local-bin-path=dist .
