.PHONY : format

HSFILES =  $(shell find . -type f -name \*.hs)

all: require build

require:
	@echo "Checking the programs required are installed..."
	@cabal --version >/dev/null 2>&1 || (echo "ERROR: cabal is required."; exit 1)

build: $(HSFILES)
	cabal build

format: $(HSFILES)
	@ormolu --mode inplace $^ && echo "Code formatted succesfully!"

clean:
	rm -rf dist-newstyle .hie
