.PHONY: all test clean cliFrontend xmppFrontend ircFrontend runXmpp runIrc

all: cliFrontend xmppFrontend ircFrontend test

cliFrontend:
	ghc --make Main.hs -o $@

xmppFrontend:
	ghc --make Xmpp/Main.hs -o $@

ircFrontend:
	ghc --make Irc/Main.hs -o $@

test:
	runhaskell Hmud/Test.hs

clean:
	find . -name '*.hi' -o -name '*.o' | xargs -r rm
	rm -f xmppFrontend cliFrontend

runXmpp: xmppFrontend
	./xmppFrontend

runIrc: ircFrontend
	./ircFrontend
