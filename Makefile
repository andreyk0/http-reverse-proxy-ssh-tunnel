
build:
	cabal build

deps:
	cabal install --dependencies-only

init: sandbox_init deps

sandbox_init:
	cabal sandbox init

clean:
	cabal clean
