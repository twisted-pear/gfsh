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

: n>s ( n -- c-addr u )
	dup >r abs s>d <# #s r> sign #> ;

: right-align ( c-addr u uX -- c-addr u )
	over over < if
		s" " pad place
		over - 0 u+do
			s"  " pad +place
		loop
		pad +place
		pad count
	else
		drop
	endif ;

128 constant max-prompt
create prompt-buffer max-prompt chars allot

: prepare-prompt ( u -- c-addr u )
	s" [" prompt-buffer place
	n>s 3 right-align prompt-buffer +place
	s" ]$ " prompt-buffer +place
	prompt-buffer count ;

: type-err ( c-addr u -- )
	['] type stderr outfile-execute ;

: add-history ( c-addr u -- )
	['] >c-string catch if
		2drop
	else
		dup readline-add_history
		free drop
	endif ;

create term? -1 ,

: read-cmdline ( c-addr u -- c-addr u f )
	term? @ if
		>c-string
	else
		2drop 0
	endif
	>r r@ readline-readline dup 0= if
		0 0
	else
		c-string> dup 0> if
			2dup add-history
		endif -1
	endif
	r> free drop ;

: free-cmdline ( c-addr u -- )
	drop libc-free ;

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
	stdin >c-fd libc-isatty term? !
	catch if
		errno> strerror type-err cr
		EXIT_FAILURE
	endif terminate ;
