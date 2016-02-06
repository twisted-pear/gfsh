struct
	double% field struct-array-type
	cell% field struct-array-size
	cell% field struct-array-data
end-struct struct-array%

: struct-array-init ( align size -- arr )
	struct-array% %allocate throw >r
	r@ struct-array-type 2!
	0 r@ struct-array-size !
	0 r@ struct-array-data !
	r> ;

: struct-array-free ( arr -- )
	dup 0<> if
		dup struct-array-data @ free drop
	endif
	free drop ;

: struct-array-i-unchecked ( u arr -- a-addr )
	dup struct-array-data @
	swap struct-array-type 2@ %size
	rot * + ;

: struct-array-i ( u arr -- a-addr )
	2dup struct-array-size @ u>= throw
	struct-array-i-unchecked ;

: struct-array-extend ( arr -- )
	>r
	r@ struct-array-data @
	r@ struct-array-size @ 1+
	r@ struct-array-type 2@ %size *
	resize throw \ Gforth specific if struct-array-data == 0.
	r@ struct-array-data !
	r@ struct-array-size @ 1+ r> struct-array-size ! ;

: struct-array-shrink ( arr -- )
	>r
	r@ struct-array-data @
	r@ struct-array-size @ 1-
	r@ struct-array-type 2@ %size *
	dup 0= if
		drop free drop 0
	else
		resize throw \ Gforth specific if struct-array-data == 0.
	endif
	r@ struct-array-data !
	r@ struct-array-size @ 1- r> struct-array-size ! ;

: struct-array-set ( a-addr u arr -- )
	2dup struct-array-size @ u>= throw
	tuck struct-array-i
	swap struct-array-type 2@ %size
	move ;

: struct-array-append ( a-addr arr -- )
	dup struct-array-size @
	over struct-array-extend
	swap struct-array-set ;

: struct-array-delete ( u arr -- )
	2dup struct-array-size @ u>= throw
	dup >r
	2dup struct-array-i
	rot 1+ dup >r
	rot dup >r struct-array-i-unchecked swap
	r@ struct-array-type 2@ %size
	r> struct-array-size @
	r> - *
	move
	r> struct-array-shrink ;

: struct-array-foreach ( xt arr -- )
	dup struct-array-size @ 0 u+do
		2dup i swap struct-array-i i swap rot execute
	loop 2drop ;
