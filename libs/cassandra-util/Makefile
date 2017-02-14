CABAL_SANDBOX ?= $(CURDIR)/.cabal-sandbox


default: clean install

init:
	cabal sandbox --sandbox=$(CABAL_SANDBOX) init

install: init
	cabal install --enable-tests --enable-bench --disable-documentation -j

clean:
	cabal clean

compile: configure
	cabal build

configure:
	cabal configure

.PHONY: dist
dist: install
	cabal sdist

distclean: clean
	-rm -rf cabal-dev
