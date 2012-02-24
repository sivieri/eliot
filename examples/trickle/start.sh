#!/bin/sh

if [ $# -eq 2 ]; then
    cd `dirname $0`
    exec erl +P 1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -name eliot@$1 -setcookie abc -s trickle start $2 false
elif [ $# -eq 3 ]; then
    cd `dirname $0`
    exec erl +P 1000000 -pa $PWD/simbin $PWD/deps/*/ebin -boot start_sasl -name eliot@$2 -setcookie abc -s trickle start $1
else
    echo "Usage: $0 qualified_name node_index | $0 config_file qualified_name node_index"
    exit 1
fi
