#!/bin/sh
set -x
cd `dirname $0`
exec erl -sname holster2 -config $PWD/config/sys.config -pa _build/default/lib/*/ebin -boot start_sasl -setcookie holster -rr