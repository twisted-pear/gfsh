gfsh
====

gfsh is a shell written in Forth.

Dependencies
------------

You'll need gforth (tested with >= 0.7.3) and libreadline (>= 6.3). The
shell has only been tested on 64-bit Linux with glibc (>= 2.22) and may
fail horribly everywhere else (or even there, YMMV).

You also need a working C toolchain to compile the C wrapper functions.

Usage
-----

Run `gforth main.fs` and pray. You should be able to leave the shell by
entering `exit`.

Features
--------

gfsh has features that shells tend to have while lacking others. Among the
things it has are

* pipes (between programs, but no redirections to/from files),
* support for variables (but no `export` builtin),
* support for escaping with `\` and `'`,
* braces (`{` and `}`),
* concatenation of commands with `;`, `&`, `&&` and `||`,
* a `cd` builtin,
* and potentially other stuff not documented here.

In the features gfsh supports it tries to be compatible with bash.

License
-------

gfsh is licensed under the GPLv3.
