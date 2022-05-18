FROM haskell:9.2.2-slim-buster AS builder

WORKDIR /app

# Download and build dependencies so they are cached by Docker:
RUN cabal v2-update
COPY dockerfile-build-secrets.cabal .
COPY Setup.hs .
RUN cabal v2-build --only-dependencies --enable-tests

# Build and test the application:
COPY test test
COPY app app
COPY src src
RUN cabal v2-test
RUN cabal v2-install


FROM alpine:3.15.4

WORKDIR /app

# Because we haven't managed to build a statically linked executable, we need some dependencies to be able to run it:
RUN apk add libc6-compat==1.2.2-r7
RUN apk add gmp==6.2.1-r1

COPY --from=builder /root/.cabal/bin/dockerfile-build-secrets .
CMD ["./dockerfile-build-secrets"]
