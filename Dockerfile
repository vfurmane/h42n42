FROM ocaml/opam:debian-12-ocaml-5.2@sha256:c5056c75484e2c2af741c5ad65d4d4497f5fc1f506d1bf1953d2181487fce189

RUN set -eux; \
    sudo apt-get update -y; \
    sudo apt-get install -y \
        bubblewrap \
        build-essential \
        curl \
        gcc \
        libgdbm-dev \
        libgmp-dev \
        libsqlite3-dev \
        libssl-dev \
        node-autoprefixer \
        node-postcss-cli \
        pkg-config \
        unzip \
        zlib1g-dev

ENV SASS_VERSION=1.79.4
RUN set -eux; \
    curl -fsSL https://github.com/sass/dart-sass/releases/download/${SASS_VERSION}/dart-sass-${SASS_VERSION}-linux-x64.tar.gz -o dart-sass-${SASS_VERSION}-linux-x64.tar.gz; \
    sudo tar -xzvf dart-sass-${SASS_VERSION}-linux-x64.tar.gz -C /usr/bin --strip-components 1 dart-sass

WORKDIR /app

COPY --chown=opam dune* h42n42.opam ./
RUN set -eux; \
    opam install .

COPY --chown=opam ./ ./
RUN set -eux; \
    eval $(opam env); \
    make all

CMD ["make", "test.byte"]
