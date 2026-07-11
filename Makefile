.PHONY: install build site watch preview clean

install:
	cabal update

build:
	cabal build

site: build
	cabal exec myblog build

watch:
	cabal run myblog -- watch

clean:
	cabal run myblog -- clean
