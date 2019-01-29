FROM ocaml/opam2:4.05
RUN opam switch 4.05+flambda 

RUN sudo apt-get update && sudo apt-get install -y \
    make \
    gcc \
    g++ \
    libc6-dev \
    libffi-dev \
    xz-utils \
    zlib1g-dev \
    git \
    gnupg \
    libgmp-dev \
    libtinfo-dev \
    m4 \
    && curl -sSL https://get.haskellstack.org/ | sh \
    && opam pin add malfunction git://github.com/stedolan/malfunction.git \
    # && sudo apt-get remove --purge --auto-remove -y m4 libgmp-dev \
    && sudo rm -rf /var/lib/apt/lists/*

ARG STACK_RESOLVER=lts-12.26
RUN stack setup --resolver=${STACK_RESOLVER}
RUN stack install idris
# WORKDIR /home/opam
# COPY --chown=opam . ./app 
# above commented in order to mount project files fixme
WORKDIR /home/opam/app
ENV OPAM_SWITCH_PREFIX="/home/opam/.opam/4.05+flambda" \
    CAML_LD_LIBRARY_PATH="/home/opam/.opam/4.05+flambda/lib/stublibs:/home/opam/.opam/4.05+flambda/lib/ocaml/stublibs:/home/opam/.opam/4.05+flambda/lib/ocaml" \
    OCAML_TOPLEVEL_PATH="/home/opam/.opam/4.05+flambda/lib/toplevel" \
    MANPATH=":/home/opam/.opam/4.05+flambda/man" \
    PATH="/home/opam/.opam/4.05+flambda/bin:/home/opam/.local/bin/:$PATH"
ENTRYPOINT [ "stack", "install", "--file-watch"]