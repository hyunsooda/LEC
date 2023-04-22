FROM ubuntu:22.04
MAINTAINER hyunsooda

# Install (1/2) cabal stack, and ghc
RUN apt-get update && \
    apt-get install wget cabal-install -y && \
    wget -qO- https://get.haskellstack.org/ | sh

# Install (2/2) LLVM-15 packages
RUN apt install lsb-release wget software-properties-common gnupg -y
RUN wget https://apt.llvm.org/llvm.sh
RUN chmod +x llvm.sh
RUN ./llvm.sh 15

ENV PATH="$PATH:/usr/lib/llvm-15/bin/"
