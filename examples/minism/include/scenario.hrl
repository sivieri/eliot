-define(SRCADDR, 16/unsigned-integer).
-record(parameter, {name = none, type = none, value = 0, fixed = false}).
-record(appliance, {ip, pid = none, params = []}).
-record(billing, {slots, cap}).
-record(slot, {starttime, endtime, priority = 0}).
% Message type
-define(BEACON, $B).
-define(APPLIANCE, $A).
-define(APPLIANCE_LOCAL, $L).
-define(COMPANY, $C).
-define(SM, $M).
-define(EVAL, $E).
-define(SCHEDULE, $S).
-define(CONSUMPTION, $O).
-define(RESET, $R).
% Parameter type
-define(PARAM_TEMPERATURE, $E).
-define(PARAM_TIME, $T).