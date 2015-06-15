FROM tattsun/haskell

WORKDIR /root

ADD ./ldn-reader.cabal /root/ldn-reader.cabal
ADD ./cabal.config /root/cabal.config
RUN cabal update && cabal install --only-dependencies

ADD ./ /root
RUN cabal update && cabal install --force-reinstalls

EXPOSE 3000

["/root/.cabal/bin/ldn-reader"]
