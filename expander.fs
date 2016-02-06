require lib.fs
require parser-common.fs
require struct-array.fs
require token.fs
require variables.fs

\ FIXME: We're misusing pstate-unclosed-ok to store our last variable assignment

: str-store-add-empty ( a-addr -- )
	heap-str% %allot
	0 over heap-str-data !
	0 over heap-str-size !
	swap ['] struct-array-append catch
	heap-str% -1 * %allot drop
	if
		1 throw
	endif ;

: str-store-extend ( c-addr u a-addr -- )
	assert( dup struct-array-size @ 0<> )
	dup struct-array-size @ 1- swap struct-array-i
	heap-str-extend ;

: str-store-take-first ( a-addr -- c-addr u )
	assert( dup struct-array-size @ 1 u<= )
	dup struct-array-size @ 0<> if
		dup >r
		0 r@ struct-array-i
		dup heap-str-data @
		swap heap-str-size @
		0 r> struct-array-delete
	else
		0 0
	endif rot drop ;

: str-store-dump ( a-addr -- c-addr1 u2 c-addr2 u2... u )
	0 over struct-array-size @ u-do
		i 1- over struct-array-i
		dup heap-str-data @
		swap heap-str-size @
		rot
	1 -loop struct-array-size @ ;

: str-store-init ( -- a-addr )
	heap-str% struct-array-init ;

: str-free ( _ a-addr -- )
	nip heap-str-data @ free drop ;

: str-store-free ( a-addr -- )
	dup 0<> if
		['] str-free over struct-array-foreach
	endif
	struct-array-free ;

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
	assert( over pstate-data @ struct-array-size @ 0= )
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
