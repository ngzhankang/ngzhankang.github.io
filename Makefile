.PHONY: build watch preview

build:
	cabal build

watch:
	cabal run myblog -- watch

preview:
	cabal run myblog -- watch
	@echo "Open http://localhost:8000 in your browser"
