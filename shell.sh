#!/bin/sh
cd `dirname $0`
exec erl -pa $PWD/ebin $PWD/deps/*/ebin -proto_dist udp -no_epmd -name $1 -setcookie abc
