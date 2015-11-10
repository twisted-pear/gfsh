require lib.fs
require pipe.fs
require run.fs

struct
	cell% field ast-left
	cell% field ast-right
	cell% field ast-func
	cell% field ast-background
	cell% field ast-stdin
	cell% field ast-stdout
	cell% field ast-n-params
	cell% field ast-params
end-struct ast%

: ast-init ( -- a-addr )
	ast% %allocate throw
	dup ast-left 0 swap !
	dup ast-right 0 swap !
	dup ast-func 0 swap !
	dup ast-background 0 swap !
	dup ast-stdin stdin swap !
	dup ast-stdout stdout swap !
	dup ast-n-params 0 swap !
	dup ast-params 0 swap ! ;

: ast-set-left ( child parent -- )
	ast-left ! ;

: ast-set-right ( child parent -- )
	ast-right ! ;

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

: ast-leaf-run ( n a-addr -- n )
	>r drop r@
	ast-dump-params
	r@ ast-stdin @ r@ ast-stdout @ stderr
	r> ast-background @
	run ;

: ast-leaf-noop ( n a-addr -- n )
	drop ;

: ast-conn-seq ( n a-addr -- n )
	swap over ast-left @ ast-exec
	swap ast-right @ ast-exec ;

: ast-conn-pipe-left ( n a-addr a a -- n )
	pipe-prepare-write
	ast-left @ ast-exec ;

: ast-conn-pipe-right ( n a-addr a a -- n )
	pipe-prepare-read
	ast-right @ ast-exec ;

: ast-conn-pipe ( n a-addr -- n )
	assert( dup ast-left @ 0<> )
	assert( dup ast-right @ 0<> )
	swap >r >r
	create-pipe 2dup
	r@ ast-pred ast-stdout !
	r@ ast-succ ast-stdin !
	r> r> swap >r r@ 2over
	['] ast-conn-pipe-left catch if
		2drop 2drop
		close-file
		close-file
		1 throw
	endif
	r> 2over
	['] ast-conn-pipe-right catch if
		2drop 2drop
		drop
		close-file
		1 throw
	endif
	rot rot 2drop ;

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
