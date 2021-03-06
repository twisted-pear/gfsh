require cfuncs.fs

0 constant EXIT_SUCCESS
1 constant EXIT_FAILURE
127 constant EXIT_NOENT

: terminate ( n -- )
	libc-exit ;

: >errno ( n -- )
	>libc-errno ;

: errno> ( -- n )
	libc-errno> ;

: fork ( -- )
	libc-fork dup 0< errno> and throw ;

: chdir ( c-addr u -- )
	>c-string dup >r
	libc-chdir
	r> free drop
	0<> errno> and throw ;

: environ> ( a -- )
	libc-environ> ;

: >environ ( -- a )
	>libc-environ ;

: putenv ( c-addrV uV c-addrN uN -- )
	>env-c-string dup libc-putenv 0<> if
		free drop
		errno> throw
	endif
	\ No free here, as putenv doesn't copy our string.
	drop ;

: exec ( a-argv a-envp -- )
	environ> >r >environ
	dup @ swap libc-execvp drop
	r> >environ
	errno> throw ;

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

: alnum? ( c -- f )
	>r
	r@ s" A" drop c@ >=
	r@ s" Z" drop c@ <=
	and
	r@ s" a" drop c@ >=
	r@ s" z" drop c@ <=
	and
	r@ s" 0" drop c@ >=
	r> s" 9" drop c@ <=
	and
	or or ;

struct
	cell% field heap-str-data
	cell% field heap-str-size
end-struct heap-str%

: extend-heap-str ( c-addr u c-addrE uE -- c-addrN uE+u )
	2tuck swap drop over + ( c-addrE uE c-addr u uE+u -- )
	dup >r swap >r ( c-addrE uE c-addr uE+u ; r: uE+u u -- )
	chars resize throw r> \ Gforth specific if c-addr == 0.
	over >r ( c-addrE uE c-addrN u ; r: uE+u c-addrN -- )
	chars + swap move
	r> r> ;

: heap-str-extend ( c-addr u a-addr -- )
	>r
	r@ heap-str-data @
	r@ heap-str-size @
	2swap extend-heap-str
	r@ heap-str-size !
	r> heap-str-data ! ;

: heap-str-free ( a-addr -- )
	heap-str-data @ free drop ;

128 constant max-prompt
create prompt-buffer max-prompt chars allot

: prepare-prompt ( u -- c-addr u )
	s" [" prompt-buffer place
	n>s 3 right-align prompt-buffer +place
	s" ]$ " prompt-buffer +place
	prompt-buffer count ;

: type-err ( c-addr u -- )
	['] type stderr outfile-execute ;

: cr-err ( -- )
	['] cr stderr outfile-execute ;

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
		errno> strerror type-err cr-err
		EXIT_FAILURE terminate
	endif
	install-sigchld-handler if
		errno> strerror type-err cr-err
		EXIT_FAILURE terminate
	endif
	mark-fds-for-closing
	stdin >c-fd libc-isatty term? !
	catch if
		errno> strerror type-err cr-err
		EXIT_FAILURE
	endif terminate ;
