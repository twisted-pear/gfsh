require cfuncs.fs
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
	['] store-args catch if
		r> free-argv
		1 throw
	endif r> ;

: consume-argv ( c-addr1 u1 c-addr2 u2... u -- )
	dup 0> if
		1- >r 2drop r> recurse
	else
		drop
	endif ;

: child-exec ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN -- )
	rot 1+
	init-argv
	dup dup @ swap libc-execvp swap
	free-argv
	drop 1 throw ;

: restore-sigint ( -- )
	SIGINT default-signal if
		errno> strerror type-err cr
		EXIT_FAILURE terminate
	endif ;

: close-fd-on-<> ( a a -- )
	over <> if
		close-file
	endif drop ;

: close-new-fds ( a a a -- )
	stderr close-fd-on-<>
	stdout close-fd-on-<>
	stdin close-fd-on-<> ;

: replace-fd ( a a -- )
	>c-fd swap >c-fd swap
	2dup libc-dup2 -1 = throw
	\ Close old fd if it is different from the new one.
	over <> if
		set-cloexec throw
	else
		drop
	endif ;

: run-program ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN a a a f -- n )
	libc-fork dup 0< if
		drop
		drop close-new-fds
		rot 1+ consume-argv
		errno>
		exit
	endif
	dup 0> if
		\ parent
		>r >r
		close-new-fds
		rot 1+ consume-argv
		r> if 
			r> drop 0
		else
			r> wait-for-child
		endif
	else
		\ child
		2drop
		restore-sigint
		stderr ['] replace-fd catch if
			errno> strerror type-err cr
			EXIT_FAILURE terminate
		endif
		stdout ['] replace-fd catch if
			errno> strerror type-err cr
			EXIT_FAILURE terminate
		endif
		stdin ['] replace-fd catch if
			errno> strerror type-err cr
			EXIT_FAILURE terminate
		endif
		\ The stack is not cleaned up here since we terminate the process anyway.
		['] child-exec catch drop
		errno> strerror type-err cr
		errno> ENOENT = if
			EXIT_NOENT
		else
			EXIT_FAILURE
		endif terminate
	endif ;

: run-builtin ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN a a a f xt -- n )
	\ TODO handle fds and maybe backgrounding
	>r
	drop
	close-new-fds
	r>
	execute ;

\ This must not throw ever, catch all errors within and make sure that errno remains 0.
: run ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN a a a f -- n )
	errno> >r 0 >errno
	>r >r >r >r
	2dup builtins search-wordlist if 
		\ builtin
		r> swap r> swap r> swap r> swap run-builtin
	else
		\ no builtin
		r> r> r> r>
		SIGCHLD block-signal
		run-program
		SIGCHLD unblock-signal
	endif
	r> >errno ;
