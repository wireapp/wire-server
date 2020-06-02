LANG := en_US.UTF-8

SHELL := /usr/bin/env bash

NAME := count-team-owners

default: all

all: install

.PHONY: clean
clean:
	stack clean $(NAME)

.PHONY: compile
compile:
	stack build .

.PHONY: install
install:
	stack install --pedantic --local-bin-path=dist .

.PHONY: fast
fast:
	stack install --fast --local-bin-path=dist .
