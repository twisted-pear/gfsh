require lib.fs

table constant builtins

get-current builtins set-current

: exit ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN -- n )
	0 terminate ;

set-current
