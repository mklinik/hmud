.PHONY: all test clean cliFrontend xmppFrontend

all: cliFrontend xmppFrontend test

cliFrontend:
	ghc --make Main.hs -o $@

xmppFrontend:
	ghc --make Xmpp/Main.hs -o $@

test:
	runhaskell Hmud/Test.hs

clean:
	find . -name '*.hi' -o -name '*.o' | xargs -r rm
	rm -f xmppFrontend cliFrontend
