#!/bin/sh

if [ $# -lt 1 ]; then
    echo "Usage: $0 qualified_name"
    exit 1
fi

cd `dirname $0`
exec erl +P 1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -name wsn@$1 -setcookie abc -s wsn
