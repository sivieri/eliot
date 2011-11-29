% Net packets
-record(routing, {pull = false		::boolean(),
				  parent = none		::atom(),
				  etx = 0			::non_neg_integer(),
				  seqno = 0			::non_neg_integer()}).
-record(data, {pull = false			::boolean(),
			   etx = 0				::non_neg_integer(),
			   payload = empty		::atom(),
			   seqno = 0			::non_neg_integer()}).
-record(ack, {id = -1				::integer()}).
% Node formats
-record(neighbor, {node				::atom(),
				   distance = 1000	::non_neg_integer(),
				   last = 0			::non_neg_integer()}).
-record(cache, {msg					::any(),
				timeout = 0			::non_neg_integer()}).
