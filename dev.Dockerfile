FROM ocaml/opam:debian-11-ocaml-5.2@sha256:a31c17f64b6f39b4dfe6abd2e769a6fd2f9022f2410ff4f442037108d24321f0

USER root:root
WORKDIR /root
ENV OPAMROOTISOK=1

RUN set -eux; \
    apt-get update -y; \
    apt-get install -y libssl-dev zlib1g-dev libgdbm-dev libgmp-dev pkg-config libsqlite3-dev
