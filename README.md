# butcher

#### Chops a command or program invocation into digestable pieces.

Similar to the `optparse-applicative` package, but less features,
more flexibility and more evil.

The main differences are:

* Provides a pure interface by default

* Has clearly defined semantics, see the section below.

* Exposes an evil monadic interface, which allows for much nicer binding of
  command part results to some variable name, where in `optparse-applicative`
  you easily lose track of what field you are modifying after the 5th `<*>`
  (admittedly, i think -XRecordWildCards improves on that issue already.)

    Evil, because you are not allowed to use the monad's full power in this
    case, i.e. there is a constraint that is not statically enforced.
    See below.

* The monadic interface allows much clearer definitions of commandparses
  with (nested) subcommands. No pesky sum-types are necessary.

* Additionally, it is possible to wrap everything in _another_ applicative
  (chosen by the user) and execute actions whenever specific parts are
  parsed successfully. This provides a direct interface for more advanced
  features, like `--no-foo` pendants to `--foo` flags.

## The evil monadic interface

As long as you only use Applicative or (Kleisli) Arrow, you can use the
interface freely. When you use Monad, there is one rule: Whenever you read
any command-parts like in

~~~~
f <- addFlag ...
p <- addParam ...
~~~~

you are only allowed to use bindings bound thusly in any command's
implemenation, i.e. inside the parameter to `addCmdImpl`. You are _not_
allowed to force/inspect/patternmatch on them before that. _good_ usage is:

~~~~
addCmdImpl $ do
  print x
  print y
~~~~

while _bad_ would be

~~~~
f <- addFlag
when f $ do
  p <- addParam
  -- evil: the existence of the param `p`
  -- depends on parse result for the flag `f`.
~~~~

That means that checking if a combination of flags is allowed must be done
after parsing. (But different commands and their subcommands have separate
sets of flags.)

## (abstract) Package intentions

Consider a commandline invocation like "ghc -O -i src -Main.hs -o Main". This
package provides a way for the programmer to simultaneously define the
semantics of your program based on its arguments and retrieve documentation
for the user. More specifically, i had three goals in mind:

1. Straight-forward description of (sub)command and flag-specific behaviour
2. Extract understandable usage/help commandline documents/texts from that
   descriptions, think of `ghc --help` or `stack init --help`.
3. Extract necessary information to compute commandline completion results
   from any partial input. (This is not implemented to any serious degree.)

## Semantics

(Sorry, this description is severely lacking, I know.)

Basic elements of a command are flags, parameters and subcommands. These can
be composed in certain ways, i.e. flags can have a (or possibly multiple?)
parameters; parameters can be grouped into sequences, and commands can have
subcommands.

Commands are essentially `String -> Either ParseError out` where `out` can
be chosen by the user. It could for example be `IO ()`.

To allow more flexible composition, the parts of a command have the "classic"
parser's type: `String -> Maybe (p, String)` where `p` depends on the part.
Parse a prefix of the input and return something and the remaining input, or
fail with `Nothing`.

A command-parser contains a sequence of parts and then a number of subcommands
and/or some implementation. 
