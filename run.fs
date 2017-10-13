require cfuncs.fs
require fd.fs
require lib.fs
require builtins.fs

: free-argv ( a-addr -- )
	dup begin
		dup @ dup while
			free drop
			cell+
	repeat 2drop
	free drop ;

: store-arg ( c-addr u a-addr -- )
	rot rot >c-string swap ! ;

: store-args ( c-addr1 u1 c-addr2 u2... a-addr u -- )
	dup >r over >r 0> if
		store-arg r> cell+ r> 1- recurse
	else
		r> r> 2drop drop
	endif ;

: alloc-argv ( u -- a-addr )
	1+ cells dup allocate throw
	dup rot erase ;

: init-argv ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	dup alloc-argv >r r@ swap
	['] store-args catch dup if
		r> free-argv
	endif throw r> ;

: consume-argv ( c-addr1 u1 c-addr2 u2... u -- )
	dup 0> if
		1- >r 2drop r> recurse
	else
		drop
	endif ;

: child-exec ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN -- n )
	rot 1+
	init-argv
	dup dup @ swap libc-execvp swap
	free-argv
	drop errno> throw ;

\ This must not throw ever, catch all errors within.
: run-program ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN list f -- n )
	SIGCHLD block-signal
	['] fork catch if
		2drop
		rot 1+ consume-argv
		SIGCHLD unblock-signal
		errno>
		exit
	endif
	dup 0> if
		\ parent
		>r >r
		drop
		rot 1+ consume-argv
		r> if 
			r> drop EXIT_SUCCESS
		else
			r> wait-for-child
		endif
		SIGCHLD unblock-signal
	else
		\ child
		2drop
		SIGINT default-signal if
			errno> strerror type-err cr-err
			EXIT_FAILURE terminate
		endif
		SIGCHLD unblock-signal
		var-list-export
		\ The stack is not cleaned up here since we terminate the process anyway.
		['] child-exec catch if
			errno> strerror type-err cr-err
			errno> ENOENT = if
				EXIT_NOENT
			else
				EXIT_FAILURE
			endif terminate
		endif
		terminate
	endif ;

\ This must not throw ever, catch all errors within.
: run-builtin ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN list f xt -- n )
	swap if
		\ run in bg
		['] fork catch if
			errno>
		else
			0= if
				execute terminate
				\ child
			endif
			EXIT_SUCCESS
		endif
		>r
		2drop
		rot 1+ consume-argv
		r>
	else
		\ run in fg
		execute
	endif ;

\ This must not throw ever, catch all errors within and make sure that errno remains 0.
: run ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN list a a a f -- n )
	errno> >r 0 >errno
	['] copy-std-fds catch if
		drop
		2drop drop
		drop
		rot 1+ consume-argv
		errno>
		r> >errno
		exit
	endif >r >r >r
	>r
	['] replace-std-fds catch if
		2drop drop
		drop
		rot 1+ consume-argv
		r> drop
		r> r> r> ['] restore-std-fds catch if
			2drop drop
		endif
		errno>
		r> >errno
		exit
	endif
	>r 2dup builtins search-wordlist if
		\ builtin
		r>
		r> rot
		run-builtin
	else
		\ no builtin
		r>
		r>
		run-program
	endif
	r> r> r> ['] restore-std-fds catch if
		2drop drop
		drop errno>
	endif
	r> >errno ;
