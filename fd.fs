require cfuncs.fs

: copy-fd ( a xt -- a )
	swap >c-fd libc-dup dup -1 = throw
	dup >r swap catch 0<> ( a 0 | x -1 -- )
	r@ set-cloexec or if
		drop
		r> libc-close drop
		1 throw
	endif
	r> drop ;

: copy-fd-r ( a -- a )
	['] c-fd>r copy-fd ;

: copy-fd-w ( a -- a )
	['] c-fd>w copy-fd ;

: replace-fd ( a a -- )
	>c-fd swap >c-fd swap
	libc-dup2 -1 = throw ;

: copy-std-fds ( -- a a a )
	stdin stdout stderr
	copy-fd-w >r
	['] copy-fd-w catch if
		drop
		r> close-file drop
		1 throw
	endif >r
	['] copy-fd-r catch if
		drop
		r> close-file drop
		r> close-file drop
		1 throw
	endif
	r> r> ;

: replace-std-fds ( a a a -- )
	stderr ['] replace-fd catch 0<> >r
	stdout ['] replace-fd catch 0<> >r
	stdin ['] replace-fd catch 0<> >r
	r> r> r> or or throw ;

: restore-std-fds ( a a a -- )
	dup >r rot dup >r rot dup >r rot
	['] replace-std-fds catch
	r> close-file drop
	r> close-file drop
	r> close-file drop
	throw ;
