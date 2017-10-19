require cstrarrays.fs
require help-words.fs
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

: var-str-deepcopy ( c-addrV uV c-addrN uN -- c-addrV2 uV2 c-addrN2 uN2 )
	dup chars allocate throw
	swap 2dup >r >r
	move
	dup chars allocate dup if
		r> r> drop free drop
	endif throw
	swap 2dup >r >r
	move
	r> r> r> r> ;

struct
	cell% field var-list-next
	cell% field var-list-table
	cell% field var-list-vars
end-struct var-list%

: var-list-init ( w next -- list )
	var-list% %allocate throw >r
	r@ var-list-next !
	r@ var-list-table !
	var% ['] struct-array-init catch dup if
		r> free drop
	endif throw
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
	throw
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

: var-list-get ( c-addrN uN list -- c-addrV uV -1 | 0 )
	assert( dup var-list-vars @ 0<> )
	var-list-find dup 0= if
		drop 0
	else
		var-get -1
	endif ;

: var-copy-to-list ( list var -- )
	swap >r
	dup var-get
	rot var-get-name
	var-str-deepcopy
	2over 2over drop nip
	r> rot rot >r >r
	['] var-list-put catch dup if
		r> free drop
		r> free drop
	endif throw
	r> r> 2drop ;

: var-list-merge ( listS listD -- )
	assert( dup var-list-vars @ 0<> )
	assert( over var-list-vars @ 0<> )
	swap var-list-vars @ ['] var-copy-to-list swap
	struct-array-foreach-with-data ;

: var-export ( var -- )
	dup var-get rot var-get-name putenv ;

: var-list-export ( list -- )
	assert( dup var-list-vars @ 0<> )
	var-list-vars @ ['] var-export swap
	struct-array-foreach ;

: var-store-envp ( a-addr var -- )
	\ get variable contents and store as C string
	dup var-get rot var-get-name >env-c-string ( a-addr c-addr -- )
	\ save address of C string in array
	over @ tuck ! ( a-addr a-addr -- )
	\ increase array index and store
	cell+ swap ! ;

: var-list-to-envp ( list -- a-addr )
	var-list-vars @ dup struct-array-size @ ( arr u -- )
	alloc-argv dup >r ( arr aN; r: aN -- )
	swap >r sp@ ['] var-store-envp r> ( aN a-aN xt arr; r: aN -- )
	['] struct-array-foreach-with-data catch dup if
		r> free-argv
	endif throw ( aNn; r: aN -- )
	drop r> ;

: envp-to-var-list ( a-addr -- list )
	0 0 var-list-init >r
	begin
		dup @ dup while
			['] env-c-string> catch dup if
				r> var-list-free
			endif throw
			['] var-str-deepcopy catch dup if
				r> var-list-free
			endif throw ( a-addr c-addrV uV c-addrN uN; r: list -- )
			2over 2over drop nip
			r@ rot rot >r >r ( a-addr c-addrV uV c-addrN uN list;
						r: list c-addrN c-addrV -- )
			['] var-list-put catch dup if
				r> free drop
				r> free drop
				r> var-list-free
			endif throw
			r> r> 2drop
			cell+
	repeat 2drop r> ;

: var-list-make-envp ( list -- a-addr )
	environ> envp-to-var-list dup >r
	['] var-list-merge catch dup if
		r> var-list-free
	endif throw
	r@ ['] var-list-to-envp catch
	r> var-list-free
	throw ;

\ if you free this and stored env vars the environ array will be fucked
table 0 var-list-init constant variables-main

variable variables-head
variables-main variables-head !

: var-push ( list -- )
	variables-head @ over var-list-next !
	variables-head ! ;

: var-pop ( -- list )
	variables-head @ var-list-next @
	assert( dup 0<> )
	variables-head @
	swap variables-head ! ;

: var-load ( c-addr u -- c-addr u )
	assert( dup 0<> )
	variables-head @
	begin
		3dup var-list-get 0= while
			var-list-next @
			dup 0= if
				\ not found
				drop
				getenv
				exit
			endif
	repeat
	\ found
	>r >r 3drop r> r> ;

: var-store ( c-addrV uV c-addrN uN -- )
	assert( dup 0<> )
	assert( variables-main 0<> )
	assert( variables-head @ 0<> )
	0 >r
	variables-head @ variables-main = if
		2dup getenv over 0<> if
			r> drop
			2over >r >r
			>r >r -1 >r
			2over 2over putenv
		else
			2drop
		endif
	endif

	variables-head @ ['] var-list-put catch dup if
		r> if
			r> r> r> r> ['] putenv catch drop
		endif
	else
		r> if r> r> r> r> 2drop 2drop endif
	endif throw ;

: var-store-var ( var -- )
	dup var-get
	rot var-get-name
	var-str-deepcopy
	2over 2over drop nip >r >r
	['] var-store catch dup if
		r> free drop
		r> free drop
	endif throw
	r> r> 2drop ;

: var-collapse ( list -- )
	variables-head @ dup variables-main = if
		drop ['] var-store-var swap var-list-vars @
		struct-array-foreach
		exit
	endif var-list-merge ;
