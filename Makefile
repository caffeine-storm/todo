SHELL:=/bin/bash

all: todo.png

TXT2DOT:=txt2dot/bin/txt2dot

# TODO: ought to be a better way to list source files for a Haskell package.
TXT2DOT_SRCS:=$(shell find txt2dot/app -name '*.hs')

todo.png: todo.dot
	dot -Tpng $^ > $@

todo.dot: todo.txt ${TXT2DOT}
	${TXT2DOT} < $< > $@

${TXT2DOT}: ${TXT2DOT_SRCS}
	mkdir -p $(dir $@)
	# TODO: if --project-file actually worked (see
	# github.com/haskell/cabal/issues/7695), we wouldn't have to `cd` here.
	cd txt2dot && cabal build
	cd txt2dot && ln -sf `cabal list-bin txt2dot | sed "s,$$(pwd)/,../,"` bin/txt2dot
