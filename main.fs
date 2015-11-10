require ast.fs
require lib.fs
require parser.fs

: main ( -- n )
	0 begin
		dup print-prompt
		read-cmdline while
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
	repeat 2drop ;

cr ' main init
