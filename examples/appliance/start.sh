#!/bin/sh

if [ $# -eq 4 ]; then
    if [ $1 = "simulate" ]; then
        cd `dirname $0`
        exec erl -pa $PWD/simbin $PWD/deps/*/simbin -proto_dist udp -no_epmd -boot start_sasl -name eliot@$2 -setcookie abc -s udp start -s appliances start $3 $4
    else
        cd `dirname $0`
        exec erl -pa $PWD/ebin $PWD/deps/*/ebin -proto_dist udp -no_epmd -boot start_sasl -name eliot@$2 -setcookie abc -s udp start -s appliances start $3 $4
    fi
else
    echo "Usage: $0 run qualified_name node_index | $0 simulate qualified_name config_file"
    exit 1
fi
