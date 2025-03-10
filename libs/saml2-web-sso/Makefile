
.PHONY: test
test:
	export SAML2_WEB_SSO_ROOT=$$(pwd) && stack test --fast

# formats all Haskell files (which don't contain CPP)
.PHONY: format
format:
	./tools/ormolu.sh

# formats all Haskell files even if local changes are not committed to git
.PHONY: formatf
formatf:
	./tools/ormolu.sh -f

# checks that all Haskell files are formatted; fail if a `make format` run is needed.
.PHONY: formatc
formatc:
	./tools/ormolu.sh -c
