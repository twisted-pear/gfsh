36 constant var-indicator \ $
82 constant var-name-max-len \ <= 82 because we're using pad
255 constant var-content-max-len \ 255 because we're using place

table constant variables

\ TODO: allow multiple chained variable tables

: >var-access ( c-addr u -- c-addr u )
	assert( dup 0<> )
	dup var-name-max-len > throw
	\ FIXME: this is super ugly but I want to use var-indicator everywhere.
	s"  " pad place
	var-indicator pad count drop c!
	pad +place
	pad count ;

: var-access? ( c-addr u -- f )
	0> if
		c@ var-indicator =
		exit
	endif
	drop 0 ;

: var-load-from-list ( c-addr u wid -- c-addr u )
	>r
	2dup var-access? 0= throw
	r> search-wordlist 0= if
		0 0
		exit
	endif
	execute @ dup 0= if
		drop
		0 0
		exit
	endif
	count ;

: var-store-to-list ( c-addr1 u1 c-addr2 u2 wid -- )
	>r
	2dup var-access? 0= throw
	2swap dup var-content-max-len > throw
	dup 1+ chars allocate throw
	dup r> swap >r >r
	place
	2dup r@ search-wordlist 0<> if
		r> drop
		dup execute @ free drop
		r> swap execute !
		2drop
		exit
	endif
	2dup nextname
	get-current variables set-current
	variable
	set-current
	r> search-wordlist 0= if
		assert( 0 )
	endif
	r> swap execute ! ;

: var-load ( c-addr u -- c-addr u )
	variables var-load-from-list ;

: var-store ( c-addr1 u1 c-addr2 u2 -- )
	variables var-store-to-list ;
