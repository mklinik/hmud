.PHONY: all tests clean cliFrontend xmppFrontend

all: cliFrontend xmppFrontend tests

cliFrontend:
	ghc --make Main.hs -o $@

xmppFrontend:
	ghc --make Xmpp/Main.hs -o $@

tests:
	runhaskell Hmud/Test.hs

clean:
	find . -name '*.hi' -o -name '*.o' | xargs -r rm
	rm -f xmppFrontend cliFrontend
