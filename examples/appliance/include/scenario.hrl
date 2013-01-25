-define(SRCADDR, 16/unsigned-integer).
-record(parameter, {name = none, type = none, value = 0, fixed = false}).
-record(appliance, {ip, pid = none, params = []}).
-record(billing, {slots, cap}).
-record(slot, {starttime, endtime, priority = 0}).
-define(BEACON, $B).
-define(APPLIANCE, $A).
-define(COMPANY, $C).
-define(SM, $M).
-define(EVAL, $E).
-define(SCHEDULE, $S).
-define(CONSUMPTION, $O).