FROM gitpod/workspace-base

RUN sudo add-apt-repository -y ppa:hvr/ghc && sudo apt-get update
RUN sudo apt-get -y install ghc-8.10.4 cabal-install-3.2
ENV PATH="/opt/ghc/bin:${PATH}"
