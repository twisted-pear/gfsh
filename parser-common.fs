struct
	cell% field pstate-next
	cell% field pstate-data
	cell% field pstate-cleanup
	cell% field pstate-closed
	cell% field pstate-unclosed-ok
	cell% field pstate-done
end-struct pstate%

: pstate-init ( a-addr -- a-addr )
	pstate% %allocate throw
	tuck pstate-next !
	dup pstate-data 0 swap !
	dup pstate-cleanup 0 swap !
	dup pstate-closed 0 swap !
	dup pstate-unclosed-ok -1 swap !
	dup pstate-done 0 swap ! ;

: pstate-drop ( a-addr -- )
	assert( dup pstate-cleanup @ 0<> )
	dup pstate-cleanup @ execute ;

: pstate-free ( a-addr -- )
	dup 0<> if
		dup pstate-next @
		dup 0<> if
			\ Drop inner ASTs as we can never reach them now.
			dup pstate-drop
			recurse
		else
			drop
		endif
	endif
	free drop ;
