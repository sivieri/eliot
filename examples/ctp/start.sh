#!/bin/sh

if [ $# -eq 3 ]; then
    if [ $1 = "simulate" ]; then
        cd `dirname $0`
        exec erl +P 1000000 -pa $PWD/simbin $PWD/deps/*/simbin -boot start_sasl -name eliot@$2 -setcookie abc -s ctp start $3
    else
        cd `dirname $0`
        exec erl +P 1000000 -pa $PWD/ebin $PWD/deps/*/ebin -boot start_sasl -name eliot@$2 -setcookie abc -s ctp start $3
    fi
else
    echo "Usage: $0 run qualified_name node_index | $0 simulate qualified_name config_file"
    exit 1
fi

