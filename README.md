# h42n42

## Usage

### Local

First install the necessary packages:

```sh
opam install .
```

Then run program with:

```sh
make test.byte
```

### With Docker

```sh
docker build -t h42n42 .
docker run \
  --rm \
  --name h42n42 \
  --publish 8080:8080 \
  h42n42
```
