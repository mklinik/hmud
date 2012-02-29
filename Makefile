default: build test

build:
	cabal build

test:
	runhaskell Hmud/Test.hs

runCli:
	cabal build && ./dist/build/cliFrontend/cliFrontend

runIrc:
	cabal build && ./dist/build/ircFrontend/ircFrontend

runXmpp:
	cabal build && ./dist/build/xmppFrontend/xmppFrontend

clean:
	rm -r dist
