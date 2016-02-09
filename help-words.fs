: 3dup ( a b c -- a b c a b c )
	0 2over rot drop 2over swap drop ;

: 3drop ( a b c -- )
	2drop drop ;
