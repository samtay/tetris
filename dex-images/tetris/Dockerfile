FROM archlinux/base

ENV DEXBUILD_DIR="/bin"

RUN mkdir -p "${DEXBUILD_DIR}"

WORKDIR ${DEXBUILD_DIR}

RUN ( \
  curl -L https://github.com/samtay/tetris/releases/download/0.1.4/tetris-`uname -s`-`uname -m` -o tetris && \
  chmod +x tetris && \
  true )

#
# container runtime configuration
#

ENTRYPOINT ["/bin/tetris"]

#
# v1 dex-api
#

LABEL \
  org.dockerland.dex.api="v1" \
  org.dockerland.dex.docker_flags="--interactive --tty" \
