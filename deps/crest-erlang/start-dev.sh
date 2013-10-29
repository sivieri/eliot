#!/bin/sh

cd `dirname $0`
exec erl  -init_debug +P  1000000 -pa $PWD/ebin $PWD/deps/*/ebin -proto_dist udp -no_epmd -name $1 -setcookie abc -boot start_sasl
