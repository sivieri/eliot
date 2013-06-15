#!/bin/sh

for i in $(seq 1 1 30)
do
   erl -pa $PWD/ebin $PWD/deps/*/ebin -proto_dist udp -no_epmd -boot start_sasl -name eliot@$1 -setcookie abc -s udp start -s sm start $2 -s sm_task test4 > /dev/null
   sleep 5
done
