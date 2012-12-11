#!/bin/sh

cd `dirname $0`
exec erl  -init_debug +P  1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -sname test 
