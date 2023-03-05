# newline

[Hackage](https://hackage.haskell.org/package/text)
[Github](https://github.com/edemko/hs-newline)

Defines a Newline data type, which is essentially a non-empty string,
but with the intention of encoding a family of algorithms
for the detection and manipulation of lines of text.
In addition, a number of pattern synonyms are also provided,
which offer a more self-documenting interface for specifying newlines.

Algorithms to split and merge text by line are also provided.
These are generalizations of Prelude's line/unline functions.
They are provided with Newline values to specify their exact behavior.

At the moment, little effort has been made to optimize the general line splitting algorithm,
and many convenience functions are missing.
This is because such improvements are premature for my personal case,
but feel free to submit a PR for either.
I'll want to make this package a central location for these tasks.

## Line Maps

This package also contains algorithms to map lines into byte ranges for a file.
See the `Text.Newline.LineMap` module.
We currently only support Unix line endings, but
  if I don't change the algorithm, the byte map should suffice for DOS line endings as well.

There is an associated file format to serialize this data.
It is simply a three-colum CSV with header row.
The columns are offset, length, and terminator, as above.
Offset and length are decimal-encoded unsigned integers.
The terminator column must hold one of the following strings:
  - @unix@ for LF (ASCII 0x0A),
  - @dos@ for CRLF (ASCOO 0x0D 0x0A) (WARNING: unimplemented),
  - @eof@ for end of file/input.
