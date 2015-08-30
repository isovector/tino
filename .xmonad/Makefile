SRC:=$(shell find src -iname '*.hs')

xmonad-x86_64-linux: ${SRC}
	cabal build exe:xmonad-tino
	mv dist/build/xmonad-tino/xmonad-tino xmonad-x86_64-linux

.PHONY: replace

