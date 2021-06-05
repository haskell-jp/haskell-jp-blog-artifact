# syntax = docker/dockerfile:experimental

FROM fumieval/ubuntu-ghc:20.04-8.10.4 as builder

WORKDIR /build

COPY docker.cabal.config /build/cabal.config
ENV CABAL_CONFIG /build/cabal.config

RUN cabal update

RUN cabal install cabal-plan \
  --constraint='cabal-plan ^>=0.7' \
  --constraint='cabal-plan +exe' \
  --installdir=/usr/local/bin

COPY *.cabal /build/
RUN --mount=type=cache,target=dist-newstyle cabal build --only-dependencies

COPY . /build

RUN --mount=type=cache,target=dist-newstyle cabal build exe:haskell-jp-blog-artifact \
  && mkdir -p /build/artifacts && cp $(cabal-plan list-bin exe:haskell-jp-blog-artifact) /build/artifacts/

RUN upx /build/artifacts/haskell-jp-blog-artifact

FROM ubuntu:20.04

RUN apt-get -yq update && apt-get -yq --no-install-suggests --no-install-recommends install \
    ca-certificates \
    curl \
    libgmp10 \
    liblzma5 \
    libssl1.1 \
    libyaml-0-2 \
    netbase \
    zlib1g \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /build/artifacts/haskell-jp-blog-artifact /app/haskell-jp-blog-artifact

ENTRYPOINT ["./haskell-jp-blog-artifact"]
