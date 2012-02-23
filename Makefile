PROJECT := ada_irc
TESTS   := ada_irc_test

all:
	gnatmake -P $(PROJECT) -Xbuild=release

debug:
	gnatmake -P $(PROJECT) -Xbuild=debug

test: all
	gnatmake -P $(TESTS)

clean:
	gnatclean -P $(PROJECT)
	gnatclean -P $(TESTS)
