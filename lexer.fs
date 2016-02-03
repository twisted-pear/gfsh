require parser-common.fs

struct
	cell% field lstate-table
	cell% field lstate-started
	cell% field lstate-open
	cell% field lstate-str-addr
	cell% field lstate-str-len
end-struct lstate%

table constant parse-special-regular
table constant expand-special-regular

: str-trim ( c-addr u xt -- c-addr u )
	>r
	begin
		dup 0= if
			r> drop
			exit
		endif
		over c@ r@ execute while
			1 /string
	repeat
	dup 0= if
		r> drop
		exit
	endif
	begin
		2dup 1- chars + c@ r@ execute while
			1-
	repeat
	r> drop ;

: char-in-table? ( c-addr w -- f )
	1 swap search-wordlist dup if
		swap drop
	endif ;

: lex-char-func ( lstate c-addr -- xt )
	1 rot lstate-table @ dup >r
	search-wordlist if
		r> drop
		exit
	endif
	s" default" r> search-wordlist
	assert( dup 0<> ) drop ;

: lex-end-func ( lstate -- xt )
	lstate-table @
	s" end" rot search-wordlist
	assert( dup 0<> ) drop ;

: lex-skip-func ( lstate -- xt )
	lstate-table @
	s" skip-char?" rot search-wordlist
	assert( dup 0<> ) drop ;

: lex-create-token-if-str ( lstate -- token -1 | 0 )
	dup lstate-str-addr @
	over lstate-str-len @
	rot lex-skip-func
	str-trim
	dup 0<> if
		create-str-token 1
	else
		2drop 0
	endif ;

: lex-begin ( c-addr u lstate -- )
	-1 over lstate-started !
	tuck lstate-str-len 0 swap !
	drop lstate-str-addr ! ;

: lex-default ( c-addr u lstate -- c-addr u token1 token2... uT )
	assert( over 0> )
	dup lstate-str-len @ 1+ swap lstate-str-len !
	1- swap char+ swap 0 ;

: lex-end ( c-addr u lstate -- c-addr u token1 token2... uT )
	assert( over 0= )
	lex-create-token-if-str
	token-type-end ['] token-init catch if
		drop if
			token-free
		endif
		1 throw
	endif swap 1+ ;

: lex-conn-char ( c-addr u lstate n -- c-addr u token1 token2... uT )
	>r
	rot rot 1 /string rot
	dup 2over rot >r >r >r
	lex-create-token-if-str
	r> r> r> lex-begin
	r> ['] token-init catch if
		drop if
			token-free
		endif
		1 throw
	endif swap 1+ ;

: lex-conn-char-dual ( c-addr u lstate n n -- c-addr u token1 token2... uT )
	>r >r
	rot rot 2dup 1 u> if
		dup c@
		swap char+ c@ 
		= if
			1- swap char+ swap
			rot r> r> nip lex-conn-char
			exit
		endif
	else
		drop
	endif
	rot r> r> drop lex-conn-char ;

: lex-sep-char ( c-addr u lstate -- c-addr u token1 token2... uT )
	rot rot 1 /string
	rot dup 2over rot >r >r >r lex-create-token-if-str
	token-type-sep ['] token-init catch if
		drop if
			token-free
		endif
		1 throw
	endif swap 1+
	r> r> r@ lex-begin
	r> dup lstate-open @ if
		0 over lstate-started !
	endif
	0 swap lstate-open ! ;

: var-name-char? ( c -- f )
	dup alnum?
	swap s" _" drop c@ =
	or ;

: lex-find-var-name ( c-addr u -- c-addr u )
	assert( dup 0<> )
	over c@ case
	s" ?" drop c@ of ( c-addr u -- c-addr u )
		drop 1
	endof
	( c-addr u -- c-addr u )
		>r
		dup 0 u+do
			over i chars + c@ var-name-char? invert if
				drop i
				leave
			endif
		loop
		r>
	endcase ;

: lex-variable ( c-addr u lstate -- c-addr u token1 token2... uT)
	rot rot 1 /string
	2dup lex-find-var-name tuck >r >r
	/string rot
	dup r> r> 2swap drop >r 2over r@ rot rot >r >r ( c-addr u c-addrV uV lstate ; r: lstate u c-addr -- )
	rot rot >r >r lex-create-token-if-str
	r> r> ['] create-var-token catch if
		2drop if
			token-free
		endif
		1 throw
	endif swap 1+
	r> r> r> lex-begin ;

get-current parse-special-regular set-current

\ TODO

: ; ( c-addr u lstate -- c-addr u token1 token2... uT )
	token-type-seq lex-conn-char ;

: & ( c-addr u lstate -- c-addr u token1 token2... uT )
	token-type-bg token-type-and lex-conn-char-dual ;

: | ( c-addr u lstate -- c-addr u token1 token2... uT )
	token-type-pipe token-type-or lex-conn-char-dual ;

: { ( c-addr u lstate -- c-addr u token1 token2... uT )
	token-type-braces-open lex-conn-char ;

: } ( c-addr u lstate -- c-addr u token1 token2... uT )
	token-type-braces-close lex-conn-char ;

: skip-char? ( c -- f )
	case
	bl of ( -- -1 ) -1 endof
	( c -- 0 ) 0 swap
	endcase ;

: default ( c-addr u lstate -- c-addr u token1 token2... uT )
	lex-default ;

: end ( c-addr u lstate -- c-addr u token1 token2... uT )
	lex-end ;

expand-special-regular set-current

\ TODO

s"  " nextname : ( c-addr u lstate -- c-addr u token1 token2... uT)
	lex-sep-char ;

: $ ( c-addr u lstate -- c-addr u token1 token2... uT)
	over 1 = if
		lex-default
		exit
	endif
	lex-variable ;

: = ( c-addr u lstate -- c-addr u token1 token2... uT)
	dup lstate-started @ if
		lex-default
		exit
	endif
	dup lstate-str-addr @
	over lstate-str-len @
	dup 0= if
		2drop
		lex-default
		exit
	endif
	dup 0 u+do
		over i chars + c@ var-name-char? invert if
			2drop
			lex-default
			unloop exit
		endif
	loop
	create-assign-token >r >r
	1 /string 2dup r@ lex-begin
	-1 r> lstate-open !
	r> 1 ;

: skip-char? ( c -- f )
	case
	( c -- 0 ) 0 swap
	endcase ;

: default ( c-addr u lstate -- c-addr u token1 token2... uT)
	lex-default ;

: end ( c-addr u lstate -- c-addr u token1 token2... uT)
	lex-end ;

set-current

: lstate-init ( w -- a-addr )
	lstate% %allocate throw
	tuck lstate-table ! ;

: lstate-free ( a-addr -- )
	free drop ;

\ FIXME: This function must not be called with a token after a token-braces-close
\	 because that might cause a use after free.
: call-parser ( token1 token2... uT pstate xt -- pstate a-addr ) recursive
	rot dup 0= if
		2drop 0
		exit
	endif
	1- swap dup >r 2swap swap >r swap ['] call-parser catch if
		r> drop
		r> token-free
		1 throw
	endif
	drop r> r> over >r catch
	r> token-free
	if
		1 throw
	endif ;

: lex-xt ( c-addr u lstate pstate xt xt -- c-addr u pstate a-addr )
	rot rot >r >r
	execute
	r> r> call-parser ;

: lex-char ( c-addr u lstate pstate xt c-addr -- c-addr u pstate a-addr )
	2over drop swap
	lex-char-func
	lex-xt ;

: lex ( c-addr u lstate xt -- a-addr )
	2over 2over drop lex-begin
	over 0 swap lstate-started !
	over 0 swap lstate-open !
	2swap 0 0
	0 >r
	begin ( lstate xt c-addr u pstate a-addr ; r: pstate -- )
		2over swap drop 0> while
			drop
			r> drop ( lstate xt c-addr u pstate ; r: -- )
			>r over >r ( lstate xt c-addr u ; r: pstate c-addr -- )
			2over r> r@ rot rot ( lstate xt c-addr u lstate pstate xt c-addr ; r: pstate -- )
			['] lex-char catch if
				r@ pstate-drop
				r> pstate-free
				1 throw
			endif
	repeat
	r> drop
	drop >r
	2swap over lex-end-func r@ rot rot ( c-addr u lstate pstate xt xt ; r: pstate -- )
	['] lex-xt catch if
		r@ pstate-drop
		r> pstate-free
		1 throw
	endif
	r> drop
	2swap 2drop
	swap pstate-free ;
