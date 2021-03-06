\c #include <errno.h>
\c #include <fcntl.h>
\c #include <signal.h>
\c #include <stdio.h>
\c #include <stdlib.h>
\c #include <string.h>
\c #include <sys/wait.h>
\c #include <unistd.h>

\c int get_errno(void) { return errno; }
\c void set_errno(int e) { errno = e; }

\ FIXME do this portably.
2 constant ENOENT

c-function libc-errno> get_errno -- n
c-function >libc-errno set_errno n -- void

\c int wait_for_child(pid_t child) {
\c	int status = 0;
\c	do {
\c		(void) waitpid(child, &status, WUNTRACED);
\c	} while (!WIFEXITED(status) && !WIFSIGNALED(status));
\c	
\c	if (WIFEXITED(status)) { return WEXITSTATUS(status); }
\c	return 128 + WTERMSIG(status);
\c }

c-function wait-for-child wait_for_child n -- n

\c void block_signal(int signal) {
\c	sigset_t mask;
\c	sigemptyset(&mask);
\c	sigaddset(&mask, signal);
\c
\c	(void) sigprocmask(SIG_BLOCK, &mask, NULL);
\c }

c-function block-signal block_signal n -- void

\c void unblock_signal(int signal) {
\c	sigset_t mask;
\c	sigemptyset(&mask);
\c	sigaddset(&mask, signal);
\c
\c	(void) sigprocmask(SIG_UNBLOCK, &mask, NULL);
\c }

c-function unblock-signal unblock_signal n -- void

\c void handle_sigchld(int signal) {
\c	pid_t chld;
\c	do {
\c		chld = waitpid(-1, NULL, WNOHANG|WUNTRACED);
\c	} while (chld > 0);
\c }

\c int install_sigchld_handler(void) {
\c	if (signal(SIGCHLD, handle_sigchld) == SIG_ERR) {
\c		return -1;
\c	}
\c	return 0;
\c }

c-function install-sigchld-handler install_sigchld_handler -- n

\c int ignore_signal(int signum) {
\c	if (signal(signum, SIG_IGN) == SIG_ERR) {
\c		return -1;
\c	}
\c	return 0;
\c }

c-function ignore-signal ignore_signal n -- n

\c int default_signal(int signum) {
\c	if (signal(signum, SIG_DFL) == SIG_ERR) {
\c		return -1;
\c	}
\c	return 0;
\c }

c-function default-signal default_signal n -- n

\ FIXME do this portably.
2 constant SIGINT
17 constant SIGCHLD

\c char** get_environ() { return environ; }
\c void set_environ(char **env) { environ = env; }

c-function libc-environ> get_environ -- a
c-function >libc-environ set_environ a -- void

c-function libc-execvp execvp a a -- n

c-function libc-fork fork -- n

c-function libc-exit exit n -- void

c-function libc-strlen strlen a -- n

: copy-to-c-string ( c-addr1 c-addr2 u -- )
	dup >r over >r move 0 r> r> chars + c! ;

: >c-string ( c-addr u -- c-addr )
	dup 1+ chars allocate throw >r r@ swap copy-to-c-string r> ;

: c-string> ( c-addr -- c-addr u )
	dup libc-strlen ;

: >env-c-string ( c-addrV uV c-addrN uN -- c-addr )
	2over nip over + 2 + chars allocate throw >r
	r@ swap copy-to-c-string
	s" =" r@ r@ libc-strlen chars + swap copy-to-c-string
	r@ r@ libc-strlen chars + swap copy-to-c-string
	r> ;

: env-c-string> ( c-addr -- c-addrV uV c-addrN uN )
	dup libc-strlen
	2dup s" =" search 0= if
		s" Invalid environment string" exception throw
	endif
	rot over -
	rot char+ rot 1-
	2swap ;

c-function libc-dup dup n -- n

c-function libc-dup2 dup2 n n -- n

c-function libc-close close n -- n

\c int set_cloexec(int fd) {
\c	int flags = fcntl(fd, F_GETFD);
\c	if (flags == -1) {
\c		return -1;
\c	}
\c	return fcntl(fd, F_SETFD, flags | FD_CLOEXEC);
\c }

c-function set-cloexec set_cloexec n -- n

\c int reset_cloexec(int fd) {
\c	int flags = fcntl(fd, F_GETFD);
\c	if (flags == -1) {
\c		return -1;
\c	}
\c	return fcntl(fd, F_SETFD, flags & ~FD_CLOEXEC);
\c }

c-function reset-cloexec reset_cloexec n -- n

\ FIXME Hack to work around different int sizes.
\c int pipe_wrapper(long fds[2]) {
\c	int pipefd[2];
\c	int ret = pipe2(pipefd, O_CLOEXEC);
\c	fds[0] = pipefd[0];
\c	fds[1] = pipefd[1];
\c	return ret;
\c }

c-function libc-pipe pipe_wrapper a -- n

c-function libc-fileno fileno a -- n
c-function libc-fdopen fdopen n a -- a

\ FIXME this is absolutely not portable

: >c-fd ( a -- n )
	libc-fileno dup -1 = libc-errno> and throw ;

: c-fd>r ( n -- a )
	s" r" >c-string >r r@
	libc-fdopen
	r> free drop
	dup 0= libc-errno> and throw ;

: c-fd>w ( n -- a )
	s" w" >c-string >r r@
	libc-fdopen
	r> free drop
	dup 0= libc-errno> and throw ;

c-function libc-isatty isatty n -- n

c-function libc-free free a -- void

c-function libc-chdir chdir a -- n

c-function libc-putenv putenv a -- n

s" readline" add-lib

\c #include <readline/readline.h>

c-function readline-readline readline a -- a

c-function readline-add_history add_history a -- void
