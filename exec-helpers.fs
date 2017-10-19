require cstrarrays.fs
require lib.fs
require variables.fs

: stack-args-exec ( c-addr1 u1 c-addr2 u2 ... u c-addrN uN list -- )
	var-list-make-envp >r
	rot 1+
	['] init-argv catch dup if
		r> free-argv
	endif throw r>
	['] exec catch
	rot rot free-argv free-argv
	throw ;
