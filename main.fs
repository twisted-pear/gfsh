require ast.fs
require lib.fs
require parser.fs

: main ( -- n )
	0 begin
		dup prepare-prompt
		read-cmdline while
			\ Save address of readline string, we have to free it later.
			swap >r r@ swap
			tokenize-cmdline
			0 >errno
			>r r@
			['] parse-cmdline catch if
				r> 2 * 1+ cleanup-stack
				errno> if
					errno> strerror type-err cr
				else
					s" Syntax error" type-err cr
				endif
			else
				r> drop
				over swap ['] ast-exec catch if 
					2drop
					errno> strerror type-err cr
				else
					nip
				endif
			endif
			\ Free readline string.
			r> free drop
	repeat 2drop ;

' main init
