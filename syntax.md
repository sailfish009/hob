# Hob Surface Syntax

The surface syntax of this language is, much like Lisp's
S-expressions, a generic way to express arbitrary structures, not
directly tied to the semantics of the language.

Contrary to S-expressions, Hob-expressions are two-layered—there is a
*pretty* syntax for writing them, and a regular syntax that this
desugars to.

The regular syntax consists of the following elements:

- EXPR := LIT | SYM | SEQ | APP

- LIT := A literal in the host language. At least string, integer, and
         float must be supported. Written `"foo"`, `"\""`, `10`, `1.5e3`.

- SYM := A string used as a symbolic identifier. Written `foo`, `+`,
         `foo-bar`, or text quoted between backticks for word
         containing characters that are not part of the
         alphanumeric/operator set.

- APP := (EXPR EXPR*) An application of the first expression to the
         sequence of (zero or more) argument expressions.

- SEQ := [EXPR*] A sequence of zero or more expressions.

So for example this could be the regular form of a simple function
definition:

    (def some-func (-> [a b] (+ a (* 2 b))))

This is very close to S-expressions, except that applications and
sequences are distinguished, which helps with disambiguation.

This structure is what the parser spits out, and what macros operate
on. It can also be used for data and configuration files, much like
S-expressions in the Lisp world.

The pretty syntax is not a strict superset of the regular syntax,
mostly because square brackets aren't used to directly express
sequences. It is an indentation-based syntax, trying to look Haskell
like without losing any genericity. Here's our `some-func` again:

    def some-func: a b ->
      a + (2 * b)

Applications are written with whitespace. `a b` means `(a b)`. Nested
applications need to be wrapped in parentheses—but note that
parentheses are a generic grouping mechanism, and do not explicitly
denote application. Operators (symbols that start and end in an
operator character), are written infix. There is no precedence, not
between operators, not between operator and regular function
application. Inner expressions are always parenthesized.

Colons are also part of the application syntax. They start a new
block, almost like an opening parenthesis, but their extent is
determined by indentation, rather than a closing parenthesis.

    def two: 1 + 1

The `1 + 1` subexpression does not have to be parenthesized because
the colon indicates that the second argument to `def` is to be parsed
as a block.

In a block, new lines that are indented further than the line above
them continue the expression above them. Expressions aligned with the
first token in the block cause the block to become a sequence, with
the aligned expression as the second argument. I.e. this text becomes
the expression below it:

    print
      "hello world"
    exit 0

    [(print "hello world") (exit 0)]

Arrows are another central concept in the syntax. Any symbol starting
with an operator character and ending with `>` is an arrow. The
expressions to the left of the arrow (parsed as if they were a
function call) are wrapped in a sequence, and become the first
argument to an application of the arrow symbol. The text after the
arrow is parsed as a block, and becomes the second argument. These are
used for pattern matches, maps, lambdas, function types, etcetera.

    a b ->
      a + b

    (-> [a b] (+ a b))

Inside parentheses or other brackets, the semicolon can be used to
create single-line sequences.

    (1; 2; 3)

    [1 2 3]

Square and curly brackets, optionally prefixed with a symbol, desugar
to calls with the (semicolon- or indentation-separated) expressions
within the brackets as arguments.

    [1; 2; 3]

    (`[]` 1 2 3)

    set{red; green; blue}

    (`set{}` red green blue)

(NOTE: This way of handling brackets is causing awkwardness in many
situations, and will probably change.)

See the test/ directory for more examples.

## Rationale

By reducing a relatively readable notation to a very small set of
primitives, the resulting data structure is easy to work with and, for
example, base a macro language on.

By using whitespace for the big structure, and parentheses for the
small structure, you get the genericity and uniformity of
S-expressions, mostly without the noise. Compare:

    (defun read-string (in quote)
      (let ((start (tstream-pos in)))
        (with-output-to-string (out)
          (loop (let ((ch (next-ch in)))
                  (cond ((not ch) (hob-stream-error in start "Unterminated string constant"))
                        ((eql ch #\\) (write-char (read-escaped-char in) out))
                        ((eql ch quote) (return))
                        (t (write-char ch out))))))))

And (transliterated pseudo-Hob code):

    def read-string: in quote ->
      def start: tstream-pos in
      with-output-to-string out:
        loop:
          match (next-ch in):
            $eof -> hob-stream-error in start "Unterminated string constant"
            "\\" -> write-char (read-escaped-char in) out
            ch &if (ch = quote) -> break
            ch -> write-char ch out

By only assigning meaning to the structure *after* parsing, the format
can be used for different purposes, and the language can be easier to
extend with new syntax.

By having an simple set of syntactic rules (current lexer and parser
together are <400 lines), the syntax is easy to get used to, and quick
to port to new environments.

## Issues

It is easy for such a syntax to become so flexible that it is hard to
read, and mistakes lead to successful parses that don't correspond to
the intended structure.

This is the reason I've removed operator precedence (though there are
situation where that leads to a silly amount of parentheses), and why
'creative' indentation that could technically be parsed is explicitly
guarded against, and given as a syntax error. For example, the below,
though unambiguous, isn't allowed (must indent a block after a colon):

    with-open-file f "/foo/bar.txt":
    do-something-with f
