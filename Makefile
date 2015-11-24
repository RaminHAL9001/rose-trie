.PHONEY: all install clean edit configure config

SOURCES := $(shell find src -type f -name '*.hs')

LIBRARY := dist/build/rose-trie

export VIM_SESSION = ./.rose-trie.vim

FILTER := \
    sed -e 's,^src/Data/Tree/RoseTrie/[^.]\+[.]hs:[0-9:]\+.*$$,& ,' \
        -e 's,[(]bound at \(src/Data/Tree/RoseTrie/[^.]\+[.]hs:[0-9:]\+\)[)],...bound at...\n\1: \n,' \
        -e 's,^[[:space:]]*at \(src/Data/Tree/RoseTrie/[^.]\+[.]hs:[0-9:]\+\),...at...\n\1: ,'

$(LIBRARY): rose-trie.cabal $(SOURCES)
	cabal build 2>&1 | $(FILTER)

install: $(LIBRARY)
	cabal install | $(FILTER)

clean:
	rm -Rf dist

config: configure

configure:
	cabal configure

edit:
	if [ -f $(VIM_SESSION) ]; \
	then vim -S $(VIM_SESSION); \
	else vim rose-trie.cabal $(SOURCES) Makefile; \
	fi;

TAGS tags: $(SOURCES)
	hasktags $(SOURCES)

