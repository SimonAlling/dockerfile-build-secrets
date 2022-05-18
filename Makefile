VERSION = 1.0.0
VERSION_WITH_V = v$(VERSION)

IMAGE_NAME = alling/dockerfile-build-secrets
IMAGE_TAG_DEV = dev
IMAGE_TAG_LATEST = latest
IMAGE_TAG_STABLE = $(VERSION)

.PHONY: all
all: build test

.PHONY: build
build:
	docker build . -t $(IMAGE_NAME):$(IMAGE_TAG_DEV)

.PHONY: test
test: build
	./test/end-to-end.sh $(IMAGE_NAME):$(IMAGE_TAG_DEV)

.PHONY: release-confirmation
release-confirmation:
	@echo -n "Publish $(VERSION_WITH_V)? [y/N] " && read ans && [ $${ans:-N} = y ]

.PHONY: release
release: build test release-confirmation
	docker tag $(IMAGE_NAME):$(IMAGE_TAG_DEV) $(IMAGE_NAME):$(IMAGE_TAG_STABLE)
	docker push $(IMAGE_NAME):$(IMAGE_TAG_STABLE)
	docker tag $(IMAGE_NAME):$(IMAGE_TAG_STABLE) $(IMAGE_NAME):$(IMAGE_TAG_LATEST)
	docker push $(IMAGE_NAME):$(IMAGE_TAG_LATEST)
	git tag $(VERSION_WITH_V)
