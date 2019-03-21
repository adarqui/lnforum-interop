build:
	stack build --fast

clean:
	stack clean

tests:
	stack test --fast

sync:
	cd ../lnforum-api-ghcjs; make sync

ghci:
	stack ghci
