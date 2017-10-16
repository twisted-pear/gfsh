require cfuncs.fs

: free-args ( a-addr -- )
	begin
		dup @ dup while
			free drop
			cell+
	repeat 2drop ;

: copy-args ( a-addrS a-addrD -- a-addrO )
	dup >r
	\ a-addrO is the address right after the last copied arg
	\ the caller can use it to copy additional strings not in a-addrS into a-addrD
	begin
		over @ dup while ( a-addrS a-addrD c-addr -- )
			\ make a deep copy of the string
			\ free previous strings if allocation fails
			c-string> ['] >c-string catch dup if
				r> free-args
			endif throw ( a-addrS a-addrD c-addr -- )
			over ! ( a-addrS a-addrD -- )
			swap cell+
			swap cell+
	repeat r> drop rot 2drop ;

: free-argv ( a-addr -- )
	dup free-args
	free drop ;

: store-arg ( c-addr u a-addr -- )
	rot rot >c-string swap ! ;

: store-args ( c-addr1 u1 c-addr2 u2... a-addr u -- )
	dup >r over >r 0> if
		store-arg r> cell+ r> 1- recurse
	else
		r> r> 2drop drop
	endif ;

: alloc-argv ( u -- a-addr )
	1+ cells dup allocate throw
	dup rot erase ;

: init-argv ( c-addr1 u1 c-addr2 u2... u -- a-addr )
	dup alloc-argv >r r@ swap
	['] store-args catch dup if
		r> free-argv
	endif throw r> ;

: argv-length? ( a-addr -- u )
	0 begin
		over @ while
			1+
			swap cell+ swap
	repeat nip ;

: consume-argv ( c-addr1 u1 c-addr2 u2... u -- )
	dup 0> if
		1- >r 2drop r> recurse
	else
		drop
	endif ;
