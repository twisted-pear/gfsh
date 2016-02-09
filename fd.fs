require cfuncs.fs

: copy-fd ( a xt -- a )
	swap >c-fd libc-dup dup -1 = libc-errno> and throw
	dup >r swap catch 0<> ( a 0 | x -1 -- )
	r@ set-cloexec or if
		drop
		r> libc-close drop
		libc-errno> throw
		assert( 0 )
	endif
	r> drop ;

: copy-fd-r ( a -- a )
	['] c-fd>r copy-fd ;

: copy-fd-w ( a -- a )
	['] c-fd>w copy-fd ;

: replace-fd ( a a -- )
	>c-fd swap >c-fd swap
	libc-dup2 -1 = libc-errno> and throw ;

: copy-std-fds ( -- a a a )
	stdin stdout stderr
	copy-fd-w >r
	['] copy-fd-w catch dup if
		swap drop
		r> close-file drop
		throw
		assert( 0 )
	endif drop >r
	['] copy-fd-r catch dup if
		swap drop
		r> close-file drop
		r> close-file drop
		throw
		assert( 0 )
	endif drop
	r> r> ;

: replace-std-fds ( a a a -- )
	stderr ['] replace-fd catch 0<> >r
	stdout ['] replace-fd catch 0<> >r
	stdin ['] replace-fd catch 0<> >r
	r> r> r> or or libc-errno> and throw ;

: restore-std-fds ( a a a -- )
	dup >r rot dup >r rot dup >r rot
	['] replace-std-fds catch
	r> close-file drop
	r> close-file drop
	r> close-file drop
	throw ;
