#!/bin/bash

INSTALL_DIR="install -d"
PKG_INSTALL_DIR="/Users/sivieri/Programs/erlang15b"
OUT_DIR="/Users/sivieri/Programs/erlang"
CP="cp -r"
SED="sed"

$INSTALL_DIR $OUT_DIR/bin
for f in epmd erl erlc escript run_erl; do \
        $CP $PKG_INSTALL_DIR/bin/$f $OUT_DIR/bin/ ; \
done
$INSTALL_DIR $OUT_DIR/lib/erlang/bin
for f in erl erlc escript run_erl start start.boot start.script start_clean.boot start_erl start_sasl.boot to_erl; do \
        $CP $PKG_INSTALL_DIR/lib/erlang/bin/$f $OUT_DIR/lib/erlang/bin/ ; \
done
$INSTALL_DIR $OUT_DIR/lib/erlang/lib
for m in erts kernel sasl stdlib; do \
        $CP $PKG_INSTALL_DIR/lib/erlang/lib/$m-* $OUT_DIR/lib/erlang/lib/ ; \
        rm -rf $OUT_DIR/lib/erlang/lib/$m-*/examples ; \
        rm -rf $OUT_DIR/lib/erlang/lib/$m-*/src ; \
done
$INSTALL_DIR $OUT_DIR/lib/erlang
$CP $PKG_INSTALL_DIR/lib/erlang/erts-* $OUT_DIR/lib/erlang/
rm -rf $OUT_DIR/lib/erlang/erts-*/{doc,include,lib,man,src}
rm -rf $OUT_DIR/lib/erlang/erts-*/bin/*.src
$INSTALL_DIR $OUT_DIR/lib/erlang/releases
$CP $PKG_INSTALL_DIR/lib/erlang/releases/* $OUT_DIR/lib/erlang/releases/
$SED 's,%ERL_ROOT%,/lib/erlang,g' \
        $OUT_DIR/lib/erlang/releases/RELEASES.src
mv -f $OUT_DIR/lib/erlang/releases/RELEASES.src \
        $OUT_DIR/lib/erlang/releases/RELEASES
for f in bin/erl bin/start erts-*/bin/erl erts-*/bin/start; do \
        $SED 's,^\(ROOTDIR\)=.*,\1=/lib/erlang,g' \
                $OUT_DIR/lib/erlang/$f ; \
done
