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
