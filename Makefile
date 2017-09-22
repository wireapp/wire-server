HASKELL_SERVICES := proxy cannon cargohold brig galley gundeck
SERVICES         := $(HASKELL_SERVICES) nginz

.PHONY: docker-services
docker-services:
	$(MAKE) -C build/alpine
	$(foreach service,$(SERVICES),$(MAKE) -C services/$(service) docker;)

