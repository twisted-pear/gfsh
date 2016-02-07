require lib.fs
require struct-array.fs

struct
	cell% field var-name-data
	cell% field var-name-size
	cell% field var-value-data
	cell% field var-value-size
end-struct var%

: var-free ( var -- )
	dup var-name-data @ free drop
	var-value-data @ free drop ;

: var-set-name ( c-addrN uN var -- )
	>r
	\ We assume that the name is only set once.
	dup chars allocate throw
	dup r@ var-name-data !
	over r> var-name-size !
	swap chars move ;

: var-get-name ( var -- c-addrN uN )
	dup var-name-data @ swap var-name-size @ ;

: var-set ( c-addrV uV var -- )
	>r
	r@ var-value-data @ free drop
	0 r@ var-value-data ! \ to prevent double free
	0 r@ var-value-size ! \ to prevent double free
	dup chars allocate throw
	dup r@ var-value-data !
	over r> var-value-size !
	swap chars move ;

: var-get ( var -- c-addrV uV )
	dup var-value-data @ swap var-value-size @ ;

struct
	cell% field var-list-next
	cell% field var-list-table
	cell% field var-list-vars
end-struct var-list%

: var-list-init ( w next -- list )
	var-list% %allocate throw >r
	r@ var-list-next !
	r@ var-list-table !
	var% ['] struct-array-init catch if
		r> free drop
		1 throw
	endif
	r@ var-list-vars !
	r> ;

: var-list-free ( list -- )
	dup 0<> if
		assert( dup var-list-vars @ 0<> )
		dup var-list-vars @
		['] var-free over struct-array-foreach
		struct-array-free
	endif
	free drop ;

: var-list-find-in-table ( c-addrN uN list -- var )
	assert( dup var-list-vars @ 0<> )
	assert( dup var-list-table @ 0<> )
	var-list-table @ search-wordlist if
		execute @
	else
		0
	endif ;

: var-list-find-in-vars ( c-addrN uN list -- var )
	assert( dup var-list-vars @ 0<> )
	var-list-vars @
	dup struct-array-size @ 0 u+do
		3dup i swap struct-array-i
		dup var-name-data @ swap var-name-size @
		str= if
			i swap 2nip struct-array-i
			unloop
			exit
		endif
	loop 2drop drop 0 ;

: var-list-find ( c-addrN uN list -- var )
	assert( dup var-list-vars @ 0<> )
	dup var-list-table @ 0= if
		var-list-find-in-vars
	else
		var-list-find-in-table
	endif ;

: var-link-from-table ( table var -- )
	swap
	2dup swap var-get-name rot search-wordlist 0= if
		over var-get-name nextname
		get-current over set-current
		variable
		set-current
		over var-get-name rot search-wordlist
		assert( dup 0<> ) drop
	else
		nip	
	endif
	execute ! ;

: var-list-put-new ( c-addrV uV c-addrN uN list -- )
	>r
	var% %allot >r
	0 r@ var-value-data !
	0 r@ var-value-size !
	r@ var-set-name
	r@ var-set
	r> r@ var-list-vars @ ['] struct-array-append catch
	var% -1 * %allot drop
	if
		1 throw
	endif
	r> dup var-list-table @ 0<> if
		dup var-list-table @ swap var-list-vars @ ['] var-link-from-table swap
		struct-array-foreach-with-data
	else
		drop
	endif ;

: var-list-put ( c-addrV uV c-addrN uN list -- )
	assert( dup var-list-vars @ 0<> )
	3dup var-list-find dup 0= if
		drop var-list-put-new
	else
		>r 3drop r> var-set
	endif ;

: var-list-get ( c-addrN uN list -- c-addrV uV )
	assert( dup var-list-vars @ 0<> )
	var-list-find dup 0= if
		drop 0 0
	else
		var-get
	endif ;

: var-list-merge ( listS listD -- )
	assert( dup var-list-vars @ 0<> )
	assert( over var-list-vars @ 0<> )
	\ TODO
	;

table 0 var-list-init constant variables-main

82 constant var-name-max-len \ <= 82 because we're using pad

\ TODO: allow multiple chained variable tables

: var-load ( c-addr u -- c-addr u )
	variables-main var-list-get ;

: var-store ( c-addr1 u1 c-addr2 u2 -- )
	variables-main var-list-put ;
