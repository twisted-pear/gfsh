struct
	cell% field token-type
	cell% field token-str-addr
	cell% field token-str-len
end-struct token%

0 constant token-type-end
1 constant token-type-str
2 constant token-type-seq
3 constant token-type-bg
4 constant token-type-pipe
5 constant token-type-and
6 constant token-type-or
7 constant token-type-braces-open
8 constant token-type-braces-close
9 constant token-type-sep
10 constant token-type-var
11 constant token-type-assign

: token-init ( n -- a-addr )
	token% %allocate throw
	swap over token-type !
	dup token-str-addr 0 swap !
	dup token-str-len 0 swap ! ;

: token-free ( a-addr -- )
	free drop ;

: create-str-token ( c-addr u -- a-addr )
	token-type-str token-init >r
	r@ token-str-len !
	r@ token-str-addr !
	r> ;

: create-var-token ( c-addr u -- a-addr)
	create-str-token
	token-type-var over token-type ! ;

: create-assign-token ( c-addr u -- a-addr)
	create-str-token
	token-type-assign over token-type ! ;
