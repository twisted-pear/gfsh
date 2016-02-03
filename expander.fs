require lib.fs
require parser-common.fs
require token.fs
require variables.fs

struct
	cell% field str-store-data
	cell% field str-store-size
end-struct str-store%

\ FIXME: We're misusing pstate-unclosed-ok to store our last variable assignment

: str-store-last-addr ( a-addr -- a-addr )
	assert( dup str-store-size @ 0<> )
	dup str-store-data @ swap str-store-size @ 1- 2* cells + ;

: str-store-get-last ( a-addr -- c-addr u )
	assert( dup str-store-size @ 0<> )
	str-store-last-addr dup cell+ swap @ swap @ ;

: str-store-set-last ( c-addr u a-addr -- )
	assert( dup str-store-size @ 0<> )
	str-store-last-addr tuck cell+ ! ! ;

: str-store-add-empty ( a-addr -- )
	>r
	r@ str-store-data @
	r@ str-store-size @ 1+ 2* cells
	resize throw \ Gforth specific if str-store-data == 0.
	r@ str-store-data !
	r@ str-store-size @ 1+ r@ str-store-size !
	0 0 r> str-store-set-last ;

: str-store-extend ( c-addr u a-addr -- )
	assert( dup str-store-size @ 0<> )
	>r
	r@ str-store-get-last
	2swap extend-heap-str
	r> str-store-set-last ;

: str-store-take-first ( a-addr -- c-addr u )
	assert( dup str-store-size @ 1 u<= )
	dup str-store-size 0<> if
		dup str-store-get-last
	else
		0 0
	endif
	rot str-store-size 0 swap ! ;

: str-store-dump ( a-addr -- )
	>r
	r@ str-store-size @
	r@ str-store-data @
	0 rot u-do
		dup i 1- 2* cells +
		dup cell+ @ swap @ swap
		rot
	1 -loop drop
	r> str-store-size @ ;

: str-store-init ( -- a-addr )
	str-store% %allocate throw
	dup str-store-data 0 swap !
	dup str-store-size 0 swap ! ;

: str-store-free ( a-addr -- )
	dup 0<> if
		dup str-store-size @ 0 u+do
			dup str-store-data @ i 2* cells + @ free drop
		loop
		dup str-store-data @ free drop
	endif
	free drop ;

create assign-buffer var-name-max-len 2 + chars allot

: expander-cleanup ( a-addr -- )
	pstate-data @ str-store-free ;

: expand-end ( pstate token -- pstate )
	assert( dup token-type @ token-type-end = )
	drop
	assert( dup pstate-data @ 0<> )
	-1 over pstate-done !
	dup pstate-unclosed-ok @ 0= if
		dup pstate-data @ str-store-take-first
		assign-buffer count >var-access var-store 
		-1 over pstate-unclosed-ok !
	endif ;

: expand-str ( pstate token -- pstate )
	assert( dup token-type @ token-type-str = )
	assert( over pstate-data @ 0<> )
	dup token-str-addr @ swap token-str-len @
	rot dup pstate-closed @ if
		dup pstate-data @ str-store-add-empty
		0 over pstate-closed !
	endif
	dup >r pstate-data @ str-store-extend
	r> ;

: expand-sep ( pstate token -- pstate )
	assert( dup token-type @ token-type-sep = )
	assert( over pstate-data @ 0<> )
	drop
	-1 over pstate-closed !
	dup pstate-unclosed-ok @ 0= if
		dup pstate-data @ str-store-take-first
		assign-buffer count >var-access var-store 
		-1 over pstate-unclosed-ok !
	endif ;

: expand-var ( pstate token -- pstate )
	assert( dup token-type @ token-type-var = )
	assert( over pstate-data @ 0<> )
	dup token-str-addr @ swap token-str-len @
	rot dup pstate-closed @ if
		dup pstate-data @ str-store-add-empty
		0 over pstate-closed !
	endif
	>r >var-access var-load r@ pstate-data @ str-store-extend
	r> ;

: expand-assign ( pstate token -- pstate )
	assert( dup token-type @ token-type-assign = )
	assert( over pstate-unclosed-ok @ )
	assert( over pstate-data @ 0<> )
	assert( over pstate-data @ str-store-size @ 0= )
	dup token-str-addr @ swap token-str-len @
	dup var-name-max-len > throw
	assign-buffer place
	0 over pstate-unclosed-ok ! ;

: expander-token-dispatcher ( pstate token -- pstate )
	dup token-type @
	case
	token-type-end of ( pstate token -- pstate )
		\ s"  END " type
		expand-end
	endof
	token-type-str of ( pstate token -- pstate )
		\ s" >" type
		\ dup token-str-addr @
		\ over token-str-len @
		\ type
		\ s" <" type
		expand-str
	endof
	token-type-sep of ( pstate token -- pstate )
		\ s"  SEP " type
		expand-sep
	endof
	token-type-var of ( pstate token -- pstate )
		\ s" VAR>" type
		\ dup token-str-addr @
		\ over token-str-len @
		\ type
		\ s" <" type
		expand-var
	endof
	token-type-assign of ( pstate token -- pstate )
		\ s" ASSIGN>" type
		\ dup token-str-addr @
		\ over token-str-len @
		\ type
		\ s" <" type
		expand-assign
	endof
	( pstate token n -- ) 1 throw
	endcase ;

: expand-cmdline ( pstate token -- pstate a-addr )
	over 0= if
		nip 0 pstate-init
		['] expander-cleanup over pstate-cleanup !
		-1 over pstate-closed !
		['] str-store-init catch if
			pstate-free
			1 throw
		endif
		over pstate-data !
		swap
	endif
	over pstate-done @ if
		1 throw
	endif
	expander-token-dispatcher
	dup pstate-data @ ;
