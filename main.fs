require ast.fs
require lexer.fs
require lib.fs
require parser.fs

: main ( -- n )
	EXIT_SUCCESS begin
		dup prepare-prompt
		read-cmdline while
			\ Save address of readline string, we have to free it later.
			2dup >r >r
			0 >errno
			parse-special-regular ['] lstate-init catch if
				drop
				errno> strerror type-err cr-err
			else
				dup >r
				['] parse-cmdline ['] lex catch
				r> lstate-free
				if
					2drop 2drop
					errno> if
						errno> strerror type-err cr-err
					else
						s" Syntax error" type-err cr-err
					endif
				else
					\ Save ast for deletion
					dup >r
					over swap ['] ast-exec catch
					r> ast-free
					if
						2drop
						errno> strerror type-err cr-err
					else
						nip
					endif
				endif
			endif
			\ Free readline string.
			r> r> free-cmdline
	repeat 2drop ;

' main init
