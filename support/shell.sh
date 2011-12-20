#!/bin/sh

IP=`ifconfig  | grep 'inet addr:'| grep -v '127.0.0.1' | cut -d: -f2 | awk '{ print $1}'`
cd `dirname $0`
exec erl -rsh ssh -setcookie abc -name master@$IP