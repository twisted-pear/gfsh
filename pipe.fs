require cfuncs.fs
require lib.fs

: convert-pipe-fds ( a-addr -- a a )
	dup @ c-fd>r
	swap
	cell+ @ c-fd>w ;

: create-pipe ( -- a a )
	2 cells allocate throw
	dup libc-pipe dup if 
		>r
		free drop
		r>
	endif throw
	dup ['] convert-pipe-fds catch dup if
		>r
		drop
		dup @ libc-close drop
		dup @ libc-close drop
		free drop
		r>
	endif throw
	rot free drop ;
