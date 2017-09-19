HASKELL_SERVICES := proxy cannon cargohold brig galley gundeck

.PHONY: docker-services
docker-services:
	$(MAKE) -C build/alpine
	$(foreach service,$(HASKELL_SERVICES),$(MAKE) -C services/$(service) docker;)

