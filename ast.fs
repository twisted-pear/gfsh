require expander.fs
require lexer.fs
require lib.fs
require fd.fs
require pipe.fs
require run.fs
require variables.fs

struct
	cell% field ast-left
	cell% field ast-right
	cell% field ast-sub
	cell% field ast-func
	cell% field ast-background
	cell% field ast-stdin
	cell% field ast-stdout
	cell% field ast-stderr
	cell% field ast-n-params
	cell% field ast-params
	cell% field ast-str-addr
	cell% field ast-str-len
end-struct ast%

: ast-init ( -- a-addr )
	ast% %allocate throw
	dup ast-left 0 swap !
	dup ast-right 0 swap !
	dup ast-sub 0 swap !
	dup ast-func 0 swap !
	dup ast-background 0 swap !
	dup ast-stdin stdin swap !
	dup ast-stdout stdout swap !
	dup ast-stderr stderr swap !
	dup ast-n-params 0 swap !
	dup ast-params 0 swap ! ;

: ast-set-left ( child parent -- )
	ast-left ! ;

: ast-set-right ( child parent -- )
	ast-right ! ;

: ast-set-sub ( child parent -- )
	ast-sub ! ;

: ast-set-func ( xt node -- )
	ast-func ! ;

: ast-set-background ( f node -- )
	ast-background ! ;

: ast-read-params ( p1 p2... u a-addr -- )
	over cells allocate throw >r
	dup ast-params @ free drop
	dup ast-params r@ swap !
	over over ast-n-params !
	drop r> swap 0 u+do
		dup rot rot !
		cell+
	loop drop ;

: ast-dump-params ( a-addr -- p1 p2... )
	dup ast-n-params @
	swap ast-params @
	0 rot u-do
		dup i 1- cells + @ swap
	1 -loop drop ;

: ast-set-str ( c-addr u node -- )
	swap over ast-str-len !
	ast-str-addr ! ;

: ast-free ( a-addr -- )
	dup 0= if
		drop exit
	endif
	dup ast-params @ free drop
	dup ast-left @ recurse
	dup ast-right @ recurse
	dup ast-sub @ recurse
	free drop ;

: ast-pred ( a-addr -- a-addr )
	dup ast-left @
	begin
		dup 0<> while
			nip dup
			ast-right @
	repeat drop ;

: ast-succ ( a-addr -- a-addr )
	dup ast-right @
	begin
		dup 0<> while
			nip dup
			ast-left @
	repeat drop ;

: update-$? ( n -- )
	n>s s" ?" var-store ;

: ast-exec ( n a-addr -- n )
	over update-$?
	dup ast-func @ execute ;

: ast-exec-bg ( n a-addr -- n )
	over update-$?
	fork 0= if
		\ child
		dup ast-func @ catch if
			2drop
			errno> strerror type-err cr-err
			EXIT_FAILURE terminate
		endif
		terminate
	else
		\ parent
		2drop EXIT_SUCCESS
	endif ;

: ast-exec-bg? ( n a-addr f -- n )
	if
		ast-exec-bg
	else
		ast-exec
	endif ;

: ast-leaf-run ( n a-addr -- n )
	>r drop
	r@ ast-str-addr @
	r@ ast-str-len @
	0 0 var-list-init var-push
	expand-special-regular lstate-init
	dup >r
	['] expand-cmdline ['] lex catch
	r> lstate-free
	dup if
		var-pop var-list-free
	endif throw
	dup >r
	str-store-dump
	r> r> swap >r >r
	dup 0= if \ variable assignments only
		drop r> drop
		r> str-store-free
		var-pop dup ['] var-collapse catch
		swap var-list-free
		throw
		0
		exit
	endif
	1- rot rot
	var-pop r> over >r >r
	r@ ast-stdin @ r@ ast-stdout @ r@ ast-stderr @
	r> ast-background @
	run
	r> var-list-free
	r> str-store-free ;

: ast-leaf-noop ( n a-addr -- n )
	drop ;

: ast-{} ( n a-addr -- n )
	assert( dup ast-left @ 0= )
	assert( dup ast-right @ 0= )
	copy-std-fds >r >r >r
	dup >r
	r@ ast-stdin @ r@ ast-stdout @ r@ ast-stderr @ ['] replace-std-fds catch dup if
		r> drop
		r> r> r> restore-std-fds
	endif throw
	ast-sub @
	r> ast-background @ ['] ast-exec-bg? catch ( n 0 | x x x >0 -- )
	r> r> r> restore-std-fds
	dup if
		>r
		2drop drop
		r>
	endif throw ;

: ast-conn-seq ( n a-addr -- n )
	swap over ast-left @ ast-exec
	swap ast-right @ ast-exec ;

: ast-conn-pipe-left ( n a-addr a a -- n )
	swap >r >r
	ast-left @ ['] ast-exec catch ( n 0 | x x >0 -- )
	r> close-file drop
	r> swap
	dup if
		>r
		close-file drop
		2drop
		r>
	endif throw
	drop ;

: ast-conn-pipe-right ( n a-addr a a -- n )
	drop >r
	ast-right @ ['] ast-exec catch ( n 0 | x x >0 -- )
	r> close-file drop
	dup if
		>r
		2drop
		r>
	endif throw ;

: ast-conn-pipe ( n a-addr -- n )
	assert( dup ast-left @ 0<> )
	assert( dup ast-right @ 0<> )
	dup >r
	create-pipe 2dup
	r@ ast-pred ast-stdout !
	r@ ast-succ ast-stdin !
	2dup >r >r
	ast-conn-pipe-left
	r> r> r> rot rot
	ast-conn-pipe-right ;

: ast-conn-and ( n a-addr -- n )
	swap over ast-left @ ast-exec
	dup 0<> if
		nip
	else
		swap ast-right @ ast-exec
	endif ;

: ast-conn-or ( n a-addr -- n )
	swap over ast-left @ ast-exec
	dup 0= if
		nip
	else
		swap ast-right @ ast-exec
	endif ;
