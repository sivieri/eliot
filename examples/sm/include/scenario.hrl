-define(SRCADDR, 16/unsigned-integer).
-record(parameter, {name = none, type = none, value = 0, fixed = false}).
-record(appliance, {name, ip, pid = none, params = []}).