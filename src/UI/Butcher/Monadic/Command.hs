-- this module only re-exports the appropriate user-facing stuff from some
-- other modules.
-- | Building-blocks of a CmdParser.
--
-- The simplest sensible CmdParser is just
--
-- > addCmdImpl $ putStrLn "hello, world!"
--
-- (assuming out is IO ()).
--
-- The empty CmdParser is also valid:
--
-- > return ()
--
-- But not very interesting - you won't get an 'out' value from this (e.g. an
-- IO-action to execute) when this matches (on the empty input).
--
-- > do
-- >   addCmd "sub" $ do
-- >     addCmdImpl $ putStrLn "sub successful"
--
-- Here, note that there is no implementation at the top-level. This means that
-- on the empty input the resulting CommandDesc has no out-value, but on "sub"
-- it has. Executed as a program, the user would be shown the usage on empty
-- input, and the putStrLn would happen on "sub".
--
-- More than one subcommand? easy:
--
-- > do
-- >   addCmd "foo" $ do {..}
-- >   addCmd "bar" $ do {..}
--
-- Basic flag usage:
--
-- > do
-- >   shouldVerbose <- addSimpleBoolFlag "v" ["verbose"] mzero
-- >   addCmdImpl $ if shouldVerbose
-- >     then putStrLn "Hello, World!!!!!"
-- >     else putStrLn "hi."
--
-- Basic param usage:
--
-- > addCmd "echo" $ do
-- >   addCmdHelpStr "print its parameter to output"
-- >   str <- addRestOfInputStringParam "STRING" (paramHelpStr "the string to print")
-- >   addCmdImpl $ putStrLn str
-- > addCmd "echoInt" $ do
-- >   i <- addReadParam "INT" mempty
-- >   addCmdImpl $ print (i::Int) -- need to disambiguate via typesig.
--
-- There are some other flag/param methods in the respective modules.
-- Also note the example at 'reorderStart'.

module UI.Butcher.Monadic.Command
  ( addCmd
  , addNullCmd
  , addCmdImpl
  , addCmdSynopsis
  , addCmdHelp
  , addCmdHelpStr
  , reorderStart
  , reorderStop
  , withReorder
  , peekCmdDesc
  , peekInput
    -- * Building CmdParsers - myprog -v --input PATH
  , module  UI.Butcher.Monadic.Flag
    -- * Building CmdParsers - myprog SOME_INT
  , module  UI.Butcher.Monadic.Param
    -- * Low-level part functions
  , addCmdPart
  , addCmdPartMany
  , addCmdPartInp
  , addCmdPartManyInp
  , ManyUpperBound (..)
  )
where



#include "prelude.inc"



import           UI.Butcher.Monadic.Internal.Types
import           UI.Butcher.Monadic.Internal.Core
import           UI.Butcher.Monadic.Flag
import           UI.Butcher.Monadic.Param



-- | Safe wrapper around 'reorderStart'/'reorderStop' for cases where reducing
-- to a single binding is possible/preferable.
withReorder :: CmdParser f out a -> CmdParser f out a
withReorder x = reorderStart *> x <* reorderStop

