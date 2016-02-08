require ast.fs
require parser-common.fs
require token.fs

\ pstate-special1 indicates if it is ok to terminate in "unclosed" state.
\ pstate-special2 is unused.

: parser-cleanup ( a-addr -- )
	pstate-data @ ast-free ;

: ast-trim ( pstate -- )
	assert( dup pstate-closed @ over pstate-special1 @ or )
	dup pstate-closed @ 0= if
		dup pstate-data @
		assert( dup ast-left @ 0<> )
		assert( dup ast-right @ 0= )
		assert( dup ast-sub @ 0= )
		dup ast-left @
		over 0 swap ast-left !
		swap ast-free
		over pstate-data !
	endif drop ;

: parse-end ( pstate token -- pstate )
	assert( dup token-type @ token-type-end = )
	drop
	dup pstate-closed @ 0= over pstate-special1 @ 0= and throw
	dup pstate-next @ 0<> throw
	-1 over pstate-done !
	dup pstate-data @ 0= if
		ast-init ['] ast-leaf-noop over ast-set-func
		over pstate-data !
		-1 over pstate-closed !
	endif
	dup ast-trim ;

: parse-str ( pstate token -- pstate )
	assert( dup token-type @ token-type-str = )
	over pstate-closed @ throw
	over pstate-closed -1 swap !
	dup token-str-addr @ swap token-str-len @
	ast-init dup >r ast-set-str
	r> ['] ast-leaf-run over ast-set-func
	over pstate-data @ 0= if
		over pstate-data !
	else
		over pstate-data @
		assert( dup ast-right @ 0= )
		ast-right !
	endif ;

: parse-conn ( pstate token xt -- pstate )
	assert( over token-type @
		dup token-type-seq = swap
		dup token-type-bg = swap
		dup token-type-pipe = swap
		dup token-type-and = swap
		dup token-type-or = swap
		drop or or or or )
	>r drop
	dup pstate-closed @ 0= throw
	0 over pstate-closed !
	0 over pstate-special1 !
	ast-init r> over ast-set-func
	over pstate-data @ over ast-set-left
	over pstate-data ! ;

: parse-braces-open ( pstate token -- pstate )
	assert( dup token-type @ token-type-braces-open = )
	drop
	dup pstate-closed @ throw
	pstate-init ;

: parse-braces-close ( pstate token -- pstate )
	assert( dup token-type @ token-type-braces-close = )
	drop
	dup pstate-closed @ 0= over pstate-special1 @ 0= and throw
	dup pstate-next @ 0= throw
	dup pstate-data @ 0= throw
	assert( dup pstate-next @ pstate-closed @ 0= )
	dup ast-trim
	ast-init ['] ast-{} over ast-set-func
	over pstate-data @ over ast-set-sub
	over pstate-next @ rot free drop swap
	over pstate-data @ 0= if
		over pstate-data !
	else
		over pstate-data @ ast-right !
	endif
	-1 over pstate-closed ! ;

: parser-token-dispatcher ( pstate token -- pstate )
	dup token-type @
	case
	token-type-end of ( pstate token -- pstate )
		\ s"  END " type
		parse-end
	endof
	token-type-str of ( pstate token -- pstate )
		\ s" >" type
		\ dup token-str-addr @
		\ over token-str-len @
		\ type
		\ s" <" type
		parse-str
	endof
	token-type-seq of ( pstate token -- pstate )
		\ s"  SEQ " type
		['] ast-conn-seq parse-conn
		-1 over pstate-special1 !
	endof
	token-type-bg of ( pstate token -- pstate )
		\ s"  BG " type
		['] ast-conn-seq parse-conn
		-1 over pstate-special1 !
		dup pstate-data @ ast-pred -1 swap ast-set-background
	endof
	token-type-pipe of ( pstate token -- pstate )
		\ s"  PIPE " type
		['] ast-conn-pipe parse-conn
	endof
	token-type-and of ( pstate token -- pstate )
		\ s"  AND " type
		['] ast-conn-and parse-conn
	endof
	token-type-or of ( pstate token -- pstate )
		\ s"  OR " type
		['] ast-conn-or parse-conn
	endof
	token-type-braces-open of ( pstate token -- pstate )
		\ s"  OPEN " type
		parse-braces-open
	endof
	token-type-braces-close of ( pstate token -- pstate )
		\ s"  CLOSE " type
		parse-braces-close
	endof
	( pstate token n -- ) 1 throw
	endcase ;

: parse-cmdline ( pstate token -- pstate a-addr )
	over 0= if
		nip 0 pstate-init
		['] parser-cleanup over pstate-cleanup !
		-1 over pstate-special1 !
		swap
	endif
	over pstate-done @ if
		1 throw
	endif
	parser-token-dispatcher
	dup pstate-data @ ;
