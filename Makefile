.PHONY: format lint install

format:
	find . -not -path '*/\.*' -name "*.hs" -exec brittany --write-mode=inplace {} \;

lint:
	hlint src/

install:
	stack install
