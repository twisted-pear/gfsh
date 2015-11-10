require ast.fs

struct
	cell% field pstate-ast
	cell% field pstate-empty
	cell% field pstate-expecting
	cell% field pstate-background
end-struct pstate%

create ps pstate% %allot drop 
0 ps pstate-ast !
-1 ps pstate-empty !
0 ps pstate-expecting !
0 ps pstate-background !

: create-ast-leaf-noop ( 0 -- a-addr )
	assert( dup 0= )
	drop
	ps pstate-background @ throw
	ast-init dup ['] ast-leaf-noop swap ast-set-func ;

: create-ast-leaf-run ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	assert( dup 0<> )
	>r r@ 1- rot rot
	r> 2* 1+
	ast-init >r r@ ast-read-params
	ps pstate-background @ r@ ast-set-background
	r> dup ['] ast-leaf-run swap ast-set-func
	0 ps pstate-background ! ;

: create-ast-conn ( xt -- a-addr )
	ast-init swap over ast-set-func ;

: parse-connector ( c-addr1 u1 c-addr2 u2... u xt -- c-addr1 u1 c-addr2 u2... u )
	>r
	dup 0= if
		r> drop
		ps pstate-empty @ if
			exit
		endif
		ps pstate-expecting @ throw
		assert( 0 )
	else
		create-ast-leaf-run
		ps pstate-empty @ if
			ps pstate-ast !
		else
			assert( ps pstate-expecting @ )
			ps pstate-ast @ ast-set-left
		endif
		r> create-ast-conn
		dup ps pstate-ast @ swap ast-set-right
		ps pstate-ast !
		-1 ps pstate-expecting !
		0 ps pstate-empty !
		0
	endif ;

table constant parse-special

get-current parse-special set-current

: ; ( c-addr1 u1 c-addr2 u2... u -- c-addr1 u1 c-addr2 u2... u )
	['] ast-conn-seq parse-connector ;

: & ( c-addr1 u1 c-addr2 u2... u -- c-addr1 u1 c-addr2 u2... u )
	['] ast-conn-seq parse-connector
	-1 ps pstate-background ! ;

: | ( c-addr1 u1 c-addr2 u2... u -- c-addr1 u1 c-addr2 u2... u )
	dup 0= if 
		ps pstate-empty @ throw
	endif
	['] ast-conn-pipe parse-connector
	-1 ps pstate-background ! ;

: && ( c-addr1 u1 c-addr2 u2... u -- c-addr1 u1 c-addr2 u2... u )
	dup 0= if 
		ps pstate-empty @ throw
	endif
	['] ast-conn-and parse-connector ;

: || ( c-addr1 u1 c-addr2 u2... u -- c-addr1 u1 c-addr2 u2... u )
	dup 0= if 
		ps pstate-empty @ throw
	endif
	['] ast-conn-or parse-connector ;

set-current

: clear-parser-state ( -- )
	ps dup pstate-empty @ 0= if
		dup pstate-ast @ ast-free
	endif
	dup pstate-empty -1 swap !
	dup pstate-expecting 0 swap !
	pstate-background 0 swap ! ;

: parse-token ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2dup parse-special search-wordlist 0= if
		\ no special char
		rot 1+
	else
		rot rot 2drop execute
	endif ;

: parse-cmdline-recursive ( c-addr1 u1 c-addr2 u2... u -- c-addr1 u1 c-addr2 u2... u )
	dup 0= if
		clear-parser-state
		exit
	endif
	rot rot >r >r 1- recurse
	r> r> parse-token ;
	
: parse-cmdline ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	parse-cmdline-recursive
	ps pstate-empty @ if
		assert( ps pstate-expecting @ 0= )
		dup 0> if
			create-ast-leaf-run
			ps pstate-ast !
		else
			create-ast-leaf-noop
			ps pstate-ast !
		endif
	else
		ps pstate-expecting @ 0= throw \ Not expecting a command but still stuff left.
		dup 0= throw \ Expecting another command but nothing left.
		create-ast-leaf-run
		ps pstate-ast @
		ast-set-left
	endif
	ps pstate-ast @ ;
