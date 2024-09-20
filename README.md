# h42n42

## Contribution

### Setup dev environment with an unprivileged user

Hopefully, you'll have access to the Docker daemon.

Build a Docker image with all the needed packages:

```sh
docker build -t dev-h42n42 -f dev.Dockerfile .
```

Create a volume to cache the _.opam_ directory:

```
docker volume create dev-h42n42-opam
```

Then run this image from the repository's root directory:

```sh
docker run \
    --rm \
    --name dev-h42n42 \
    -it \
    --publish 8080:8080 \
    --volume dev-h42n42-opam:/root/.opam \
    --volume $PWD:/root/h42n42 \
    dev-h42n42 \
    bash
```

Finally, you may change current directory to _h42n42_ and install OPAM packages:

```
cd h42n42
opam install . --with-test --with-doc --deps-only
```
