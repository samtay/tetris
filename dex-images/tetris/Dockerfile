FROM haskell:8

ENV DEXBUILD_DIR="/build"

RUN mkdir -p "${DEXBUILD_DIR}"

WORKDIR ${DEXBUILD_DIR}

RUN ( \
  git clone "https://github.com/samtay/tetris.git" && \
  cd tetris && \
  git checkout tags/0.1.0 && \
  rm -rf .git && \
  mkdir bin && \
  stack install --local-bin-path bin && \
  true )

#
# container runtime configuration
#

ENTRYPOINT ["/build/tetris/bin/tetris"]

#
# v1 dex-api
#

LABEL \
  org.dockerland.dex.api="v1" \
  org.dockerland.dex.docker_flags="--interactive --tty" \
