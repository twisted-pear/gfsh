require lib.fs

table constant builtins

get-current builtins set-current

: exit ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN -- n )
	0 terminate ;

: cd ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN -- n )
	2drop
	dup 0= if
		s" HOME" getenv
	else
		1- rot rot
	endif
	>r >r
	2* cleanup-stack
	r> r>
	dup 0= if
		2drop
		s" HOME not set" type-err cr-err
		EXIT_FAILURE
		exit
	endif
	['] chdir catch if
		2drop
		errno> strerror type-err cr-err
		EXIT_FAILURE
	else
		EXIT_SUCCESS
	endif ;

set-current
