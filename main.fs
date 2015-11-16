require ast.fs
require lib.fs
require parser.fs

: main ( -- n )
	0 begin
		dup prepare-prompt
		read-cmdline while
			\ Save address of readline string, we have to free it later.
			2dup >r >r
			tokenize-cmdline
			0 >errno
			>r r@
			['] parse-cmdline catch if
				r> 2 * 1+ cleanup-stack
				errno> if
					errno> strerror type-err cr-err
				else
					s" Syntax error" type-err cr-err
				endif
			else
				r> drop
				\ Save ast for deletion
				>r r@
				over swap ['] ast-exec catch if 
					2drop
					errno> strerror type-err cr-err
				else
					nip
				endif
				\ Free ast
				r> ast-free
			endif
			\ Free readline string.
			r> r> free-cmdline
	repeat 2drop ;

' main init
