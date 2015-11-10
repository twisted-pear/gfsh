require cfuncs.fs
require lib.fs

: convert-pipe-fds ( a-addr -- a a )
	dup @ c-fd>r
	swap
	cell+ @ c-fd>w ;

: create-pipe ( -- a a )
	2 cells allocate throw
	dup libc-pipe if 
		free drop
		1 throw
	endif
	dup ['] convert-pipe-fds catch if
		drop
		dup @ libc-close drop
		dup @ libc-close drop
		free drop
		1 throw
	endif
	rot free drop ;

: pipe-prepare-write ( a a -- )
	drop
	>c-fd set-cloexec throw ;

: pipe-prepare-read ( a a -- )
	drop
	>c-fd reset-cloexec throw ;
