FROM haskell:8

WORKDIR /opt/app

# Configure GHCi
COPY config /opt/app/config
RUN touch config/.ghc/ghci_history && \
  cd ~ && \
  cp -r --symbolic-link /opt/app/config/. . && \
  cd -

COPY . /opt/app
