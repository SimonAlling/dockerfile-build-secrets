# dockerfile-build-secrets

This program reads the content of a Dockerfile from standard input and prints a JSON object containing all build secrets it references and whether they are required or not.

## Usage

Take for example this Dockerfile:

```dockerfile
FROM alpine

RUN --mount=type=secret,id=foo echo
RUN --mount=type=secret,id=bar,required=true echo
```

Extract its build secrets like this:

```console
$ docker run -i alling/dockerfile-build-secrets < Dockerfile
{"bar":{"required":true},"foo":{"required":false}}
```

## Contribute

Build and test:

```shell
make
```

Build and run manually:

```shell
docker build -t alling/dockerfile-build-secrets .
docker run -i alling/dockerfile-build-secrets < Dockerfile
```

For a smooth development experience, you'll probably need some Haskell extension in your IDE.
With a bit of luck, you may be able to install the basic prerequisites (GHC, Cabal, …) using [GHCUp](https://www.haskell.org/ghcup/).

### Adding a test case

All files in `test/testdata/` are automatically picked up by the test suite, so adding a test case is as simple as creating a Dockerfile, along with a file containing its expected output, in that directory.

### Publishing a release

```shell
make release
```
