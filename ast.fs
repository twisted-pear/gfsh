require lib.fs
require fd.fs
require pipe.fs
require run.fs

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

: ast-exec ( n a-addr -- n )
	dup ast-func @ execute ;

: ast-exec-bg ( n a-addr -- n )
	fork 0= if
		\ child
		dup ast-func @ catch if
			2drop
			errno> strerror type-err cr
			EXIT_FAILURE terminate
		endif
		terminate
	else
		\ parent
		2drop 0
	endif ;

: ast-exec-bg? ( n a-addr f -- n )
	if
		ast-exec-bg
	else
		ast-exec
	endif ;

: ast-leaf-run ( n a-addr -- n )
	>r drop r@
	ast-dump-params
	r@ ast-stdin @ r@ ast-stdout @ r@ ast-stderr @
	r> ast-background @
	run ;

: ast-leaf-noop ( n a-addr -- n )
	drop ;

: ast-{} ( n a-addr -- n )
	assert( dup ast-left @ 0= )
	assert( dup ast-right @ 0= )
	copy-std-fds >r >r >r
	dup >r
	r@ ast-stdin @ r@ ast-stdout @ r@ ast-stderr @ ['] replace-std-fds catch if
		r> drop
		r> r> r> restore-std-fds
		1 throw
	endif
	ast-sub @
	r> ast-background @ ['] ast-exec-bg? catch ( n 0 | x x x >0 -- )
	r> r> r> restore-std-fds
	if
		2drop drop
		1 throw
	endif ;

: ast-conn-seq ( n a-addr -- n )
	swap over ast-left @ ast-exec
	swap ast-right @ ast-exec ;

: ast-conn-pipe-left ( n a-addr a a -- n )
	swap >r >r
	ast-left @ ['] ast-exec catch ( n 0 | x x >0 -- )
	r> close-file drop
	r> swap
	if
		close-file drop
		2drop
		1 throw
	endif
	drop ;

: ast-conn-pipe-right ( n a-addr a a -- n )
	drop >r
	ast-right @ ['] ast-exec catch ( n 0 | x x >0 -- )
	r> close-file drop
	if
		2drop
		1 throw
	endif ;

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
