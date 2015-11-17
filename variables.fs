36 constant var-indicator \ $

table constant variables

: var-access? ( c-addr u -- f )
	0> if
		c@ var-indicator =
		exit
	endif
	drop 0 ;

: var-load ( c-addr u -- c-addr u )
	2dup var-access? 0= throw
	variables search-wordlist 0= if
		0 0
		exit
	endif
	execute @ dup 0= if
		drop
		0 0
		exit
	endif
	count ;

: var-store ( c-addr1 u1 c-addr2 u2 -- )
	2dup var-access? 0= throw
	2swap dup 255 > throw
	dup 1+ chars allocate throw
	dup >r
	place
	2dup variables search-wordlist 0<> if
		dup execute @ free drop
		r> swap execute !
		2drop
		exit
	endif
	2dup nextname
	get-current variables set-current
	variable
	set-current
	variables search-wordlist 0= if
		assert( 0 )
	endif
	r> swap execute ! ;
