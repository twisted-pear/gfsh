require cfuncs.fs

0 constant EXIT_SUCCESS
1 constant EXIT_FAILURE
127 constant EXIT_NOENT

: terminate ( n -- )
	libc-exit ;

: >errno ( n -- )
	>libc-errno ;

: errno> ( n -- )
	libc-errno> ;

4096 constant max-line
create line-buffer max-line chars allot

: print-prompt ( u -- )
	s" [" type 3 u.r s" ]$ " type ;

: type-err ( c-addr u -- )
	['] type stderr outfile-execute ;

: read-cmdline ( -- c-addr u f )
	line-buffer max-line stdin read-line throw
	line-buffer rot rot ;

: skip-spaces ( c-addr u -- u )
	0 over u-do
		2dup 1- chars + c@ bl <> if 
			leave
		endif
		1-
	1 -loop nip ;

: get-param ( c-addr u -- c-addr u )
	tuck 0 over u-do
		2dup 1- chars + c@ bl = if 
			leave
		endif
		1-
	1 -loop
	dup rot rot chars + rot rot - ;

: tokenize-cmdline ( c-addrX uX -- c-addr1 u1 c-addr2 u2... u )
	0 >r begin
		dup 0> while
			2dup skip-spaces swap drop
			2dup get-param dup >r 2swap
			r> -
			r> 1+ >r
	repeat 2drop
	r@ if
		dup 0= if
			2drop r> 1- >r
		endif
	endif r> ;

: cleanup-stack ( p1 p2... u -- )
	0 u+do
		drop
	loop ;

\ Set FD_CLOEXEC on all fds except for stdin, stdout and stderr to avoid leaking them to children.
: mark-fds-for-closing ( -- )
	3 begin
		dup set-cloexec 0= while
			1+
	repeat drop ;

: init ( xt -- )
	SIGINT ignore-signal if
		errno> strerror type-err cr
		EXIT_FAILURE terminate
	endif
	install-sigchld-handler if
		errno> strerror type-err cr
		EXIT_FAILURE terminate
	endif
	mark-fds-for-closing
	catch if
		errno> strerror type-err cr
		EXIT_FAILURE
	endif terminate ;
