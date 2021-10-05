FROM docker.io/library/haskell:8-buster
WORKDIR /opt/haskventure/

# Don't run as root
RUN adduser --disabled-password haskventure --gecos Haskventure,,, \
    && chown haskventure /opt/haskventure/
USER haskventure

# install dependencies
RUN cabal update
COPY ./haskventure.cabal ./
RUN cabal build --only-dependencies -j4

# install the game
COPY . /opt/haskventure/
RUN cabal install

CMD ["/home/haskventure/.cabal/bin/haskventure"]

# When running the container, mount any external adventures into
# subdirectories of /opt/haskventure/adventures/ so the game can find
# them.
