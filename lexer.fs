require parser-common.fs

struct
	cell% field lstate-table
	cell% field lstate-started
	cell% field lstate-open
	cell% field lstate-str-addr
	cell% field lstate-str-len
	cell% field lstate-str-trail-len
end-struct lstate%

table constant parse-special-regular
table constant parse-special-'
table constant expand-special-regular
table constant expand-special-'

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

: lex-create-token-if-str ( lstate -- token 1 | 0 )
	>r
	r@ lstate-str-addr @
	r@ lstate-str-len @
	r@ lstate-str-trail-len @
	assert( 2dup u>= )
	0 r> lstate-str-trail-len !
	- dup 0<> if
		create-str-token 1
	else
		2drop 0
	endif ;

: lex-begin ( c-addr u lstate -- )
	-1 over lstate-started !
	0 over lstate-str-trail-len !
	tuck lstate-str-len 0 swap !
	drop lstate-str-addr ! ;

: lex-default ( c-addr u lstate -- c-addr u token1 token2... uT )
	assert( over 0<> )
	0 over lstate-str-trail-len !
	dup lstate-str-len @ 1+ swap lstate-str-len !
	1 /string 0 ;

: lex-default-skip ( c-addr u lstate -- c-addr u token1 token2... uT )
	assert( over 0> )
	rot dup c@ >r rot rot
	r> over lex-skip-func execute if
		dup lstate-str-len @ 0= if
			dup lstate-str-addr @ char+
			swap lstate-str-addr !
			1 /string 0
			exit
		else
			dup lstate-str-trail-len @ 1+
			over lstate-str-trail-len !
		endif
	endif
	lex-default ;

: lex-end ( c-addr u lstate -- c-addr u token1 token2... uT )
	assert( over 0= )
	lex-create-token-if-str
	token-type-end ['] token-init catch dup if
		>r
		drop if
			token-free
		endif
		r>
	endif throw swap 1+ ;

: lex-conn-char ( c-addr u lstate n -- c-addr u token1 token2... uT )
	>r
	rot rot 1 /string rot
	dup 2over rot >r >r >r
	lex-create-token-if-str
	r> r> r> lex-begin
	r> ['] token-init catch dup if
		>r
		drop if
			token-free
		endif
		r>
	endif throw swap 1+ ;

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
	token-type-sep ['] token-init catch dup if
		>r
		drop if
			token-free
		endif
		r>
	endif throw swap 1+
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
	r> r> ['] create-var-token catch dup if
		>r
		2drop if
			token-free
		endif
		r>
	endif throw swap 1+
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

: \ ( c-addr u lstate -- c-addr u token1 token2... uT )
	assert( over 0<> )
	over 1 = if
		\ TODO: support linebreak here?
		drop
		1 /string 0
		exit
	endif
	dup >r
	lex-default drop
	r> lex-default ;

: ' ( c-addr u lstate -- c-addr u token1 token2... uT )
	dup >r
	lex-default
	parse-special-' r> lstate-table ! ;

: skip-char? ( c -- f )
	case
	bl of ( -- -1 ) -1 endof
	( c -- 0 ) 0 swap
	endcase ;

: default ( c-addr u lstate -- c-addr u token1 token2... uT )
	lex-default-skip ;

: end ( c-addr u lstate -- c-addr u token1 token2... uT )
	lex-end ;

parse-special-' set-current

: ' ( c-addr u lstate -- c-addr u token1 token2... uT )
	dup >r
	lex-default
	parse-special-regular r> lstate-table ! ;

: skip-char? ( c -- f )
	case
	( c -- 0 ) 0 swap
	endcase ;

: default ( c-addr u lstate -- c-addr u token1 token2... uT )
	lex-default-skip ;

: end ( c-addr u lstate -- c-addr u token1 token2... uT )
	s" missing '" exception throw ;

expand-special-regular set-current

\ TODO

s"  " nextname : ( c-addr u lstate -- c-addr u token1 token2... uT)
	lex-sep-char ;

: $ ( c-addr u lstate -- c-addr u token1 token2... uT)
	over 1 = if
		lex-default
		exit
	endif
	rot rot 2dup 1 /string
	lex-find-var-name swap drop 0= if
		rot lex-default
		exit
	endif rot
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

: \ ( c-addr u lstate -- c-addr u token1 token2... uT )
	swap 1- tuck 0<> if
		>r 2dup
		over char+ rot rot move
		r> lex-default
	else
		drop 0
	endif ;

: ' ( c-addr u lstate -- c-addr u token1 token2... uT )
	expand-special-' swap lstate-table !
	1- 2dup
	over char+ rot rot move
	0 ;

: skip-char? ( c -- f )
	case
	( c -- 0 ) 0 swap
	endcase ;

: default ( c-addr u lstate -- c-addr u token1 token2... uT)
	lex-default-skip ;

: end ( c-addr u lstate -- c-addr u token1 token2... uT)
	lex-end ;

expand-special-' set-current

: ' ( c-addr u lstate -- c-addr u token1 token2... uT )
	expand-special-regular swap lstate-table !
	1- 2dup
	over char+ rot rot move
	0 ;

: skip-char? ( c -- f )
	case
	( c -- 0 ) 0 swap
	endcase ;

: default ( c-addr u lstate -- c-addr u token1 token2... uT )
	lex-default-skip ;

: end ( c-addr u lstate -- c-addr u token1 token2... uT )
	assert( 0 ) ;

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
	1- swap dup >r 2swap swap >r swap ['] call-parser catch dup if
		r> drop
		r> token-free
	endif throw
	drop r> r> over >r catch
	r> token-free
	throw ;

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
			['] lex-char catch dup if
				r@ pstate-drop
				r> pstate-free
			endif throw
	repeat
	r> drop
	drop >r
	2swap over lex-end-func r@ rot rot ( c-addr u lstate pstate xt xt ; r: pstate -- )
	['] lex-xt catch dup if
		r@ pstate-drop
		r> pstate-free
	endif throw
	r> drop
	2swap 2drop
	swap pstate-free ;
