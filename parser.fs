require ast.fs
require variables.fs

struct
	cell% field pstate-ast
	cell% field pstate-empty
	cell% field pstate-expecting
	cell% field pstate-background
	cell% field pstate-depth
	cell% field pstate-special-table
end-struct pstate%

create ps pstate% %allot drop 
0 ps pstate-ast !
-1 ps pstate-empty !
0 ps pstate-expecting !
0 ps pstate-background !
0 ps pstate-depth !

: pstate-dump ( -- a1 a2... u )
	ps pstate-ast @
	ps pstate-empty @
	ps pstate-expecting @
	ps pstate-background @
	ps pstate-depth @
	ps pstate-special-table @
	6 ;

: pstate-restore ( a1 a2... u )
	assert( dup 6 = )
	drop
	ps pstate-special-table !
	ps pstate-depth !
	ps pstate-background !
	ps pstate-expecting !
	ps pstate-empty !
	ps pstate-ast ! ;

defer parse-cmdline ( c-addr1 u1 c-addr2 u2... u -- a-addr )

: create-ast-leaf-noop ( 0 -- a-addr )
	assert( dup 0= )
	drop
	ps pstate-background @ throw
	ast-init dup ['] ast-leaf-noop swap ast-set-func ;

: create-ast-leaf-run ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	assert( dup 0<> )
	dup 2* 1+
	ast-init >r r@ ast-read-params
	ps pstate-background @ r@ ast-set-background
	r> dup ['] ast-leaf-run swap ast-set-func
	0 ps pstate-background ! ;

: create-ast-{} ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	assert( dup 0<> )
	ast-init >r r@ ['] ast-{} swap ast-set-func
	ps pstate-background @ r@ ast-set-background
	0 ps pstate-background !
	pstate-dump dup begin
		dup 0> while rot >r 1-
	repeat drop >r
	['] parse-cmdline catch
	r> dup begin
		dup 0> while r> rot rot 1-
	repeat drop pstate-restore
	if
		r> ast-free
		1 throw
	endif
	r@ ast-set-sub
	r> ;


: create-ast-conn ( xt -- a-addr )
	ast-init swap over ast-set-func ;

: parse-connector ( c-addr1 u1 c-addr2 u2... u xt -- c-addr1 u1 c-addr2 u2... u )
	>r
	dup 0= if
		ps pstate-empty @ if
			r> drop
			exit
		endif
		ps pstate-expecting @ throw
		r> create-ast-conn
		dup ps pstate-ast @ swap ast-set-right
		ps pstate-ast !
		-1 ps pstate-expecting !
	else
		create-ast-leaf-run
		ps pstate-empty @ if
			ps pstate-ast !
		else
			ps pstate-expecting @ 0= if 
				ast-free
				1 throw
			endif
			ps pstate-ast @ ast-set-left
		endif
		r> create-ast-conn
		dup ps pstate-ast @ swap ast-set-right
		ps pstate-ast !
		-1 ps pstate-expecting !
		0 ps pstate-empty !
		0
	endif ;

table constant parse-special-regular
table constant parse-special-braces

get-current parse-special-regular set-current

: variable? ( c-addr u -- f )
	var-access? ;

: ; ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2drop
	['] ast-conn-seq parse-connector ;

: & ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2drop
	['] ast-conn-seq parse-connector
	-1 ps pstate-background ! ;

: | ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2drop
	dup 0= if 
		ps pstate-empty @ throw
	endif
	['] ast-conn-pipe parse-connector
	-1 ps pstate-background ! ;

: && ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2drop
	dup 0= if 
		ps pstate-empty @ throw
	endif
	['] ast-conn-and parse-connector ;

: || ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2drop
	dup 0= if 
		ps pstate-empty @ throw
	endif
	['] ast-conn-or parse-connector ;

: { ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	1 throw ;

: } ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2drop
	assert( ps pstate-depth @ 0= )
	dup 0<> if
		1 throw
	endif
	assert( ps pstate-empty @ ps pstate-expecting @ or )
	1 ps pstate-depth !
	parse-special-braces ps pstate-special-table ! ;

parse-special-braces set-current

: variable? ( c-addr u -- f )
	2drop
	0 ;

: { ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	assert( ps pstate-depth @ 0> )
	ps pstate-depth @ 1- dup ps pstate-depth !
	0> if
		rot 1+
		exit
	endif
	2drop
	dup 0= throw
	assert( ps pstate-empty @ ps pstate-expecting @ or )
	parse-special-regular ps pstate-special-table !
	create-ast-{}
	ps pstate-empty @ if
		ps pstate-ast !
	else
		assert( ps pstate-expecting @ )
		ps pstate-ast @ ast-set-left
	endif
	0 ps pstate-expecting !
	0 ps pstate-empty !
	0 ;

: } ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	rot 1+
	ps pstate-depth @ 1+ ps pstate-depth ! ;

set-current

parse-special-regular ps pstate-special-table !

: clear-parser-state ( -- )
	0 ps pstate-ast !
	-1 ps pstate-empty !
	0 ps pstate-expecting !
	0 ps pstate-background !
	0 ps pstate-depth !
	parse-special-regular ps pstate-special-table ! ;

: parse-token ( c-addr1 u1 c-addr2 u2... u c-addrN uN -- c-addr1 u1 c-addr2 u2... u )
	2dup s" variable?" ps pstate-special-table @ search-wordlist
	assert( dup 0<> )
	drop execute if
		rot 1+
		\ insert marker for AST
		0 0 rot 1+
		exit
	endif
	2dup ps pstate-special-table @ search-wordlist 0= if
		\ no special char
		rot 1+
	else
		execute
	endif ;

: parse-cmdline-recursive ( c-addr1 u1 c-addr2 u2... u -- c-addr1 u1 c-addr2 u2... u )
	dup 0= if
		exit
	endif
	rot rot >r >r 1- recurse
	r> r> parse-token ;

: parse-cmdline-tail ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	ps pstate-depth @ throw
	ps pstate-empty @ if
		assert( ps pstate-expecting @ 0= )
		dup 0> if
			create-ast-leaf-run
		else
			create-ast-leaf-noop
		endif
		ps pstate-ast !
		0 ps pstate-empty !
	else
		ps pstate-expecting @ if
			dup 0= throw \ Expecting another command but nothing left.
			create-ast-leaf-run
			ps pstate-ast @ ast-set-left
		else
			throw
		endif
	endif
	ps pstate-ast @ ;

: parse-cmdline-catch ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	parse-cmdline-recursive
	parse-cmdline-tail ;

: parse-cmdline-real ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	clear-parser-state
	['] parse-cmdline-catch catch if
		ps pstate-empty @ 0= if
			ps pstate-ast @ ast-free
		endif
		1 throw
	endif ;

' parse-cmdline-real is parse-cmdline
