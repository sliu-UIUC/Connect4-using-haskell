# Commands:

.PHONY: build init test clean doc deploy stage
EXECUTABLE=cluster

build: Connect4.hs
	ghc --make -O -o connect4 Main.hs

prof:
	ghc --make -prof -o connect4 Main.hs

all: build test

# Cleaning commands:
clean:
	rm -f connect4
	rm -f *.hi
	rm -f *.o

test: build
	./connect4 --test

testq: build
	./connect4 --test -q

setup:
	cabal install ansi-terminal
	cabal install drawille
