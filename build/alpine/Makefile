
default: deps builder

.PHONY: deps
deps:
	docker build -t wire-server-deps:alpine -f Dockerfile.deps .

.PHONY: builder
builder:
	docker build -t wire-server-builder:alpine -f Dockerfile.builder .
