SRC:=$(shell find src -iname '*.hs')

xmonad-x86_64-linux: ${SRC}
	stack build
	cp .stack-work/install/x86_64-linux/lts-5.2/7.10.3/bin/xmonad-tino xmonad-x86_64-linux

.PHONY: replace

