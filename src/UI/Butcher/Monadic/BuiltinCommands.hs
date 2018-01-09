-- | Some CmdParser actions that add predefined commands.
module UI.Butcher.Monadic.BuiltinCommands
  ( addHelpCommand
  , addHelpCommandShallow
  , addButcherDebugCommand
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS

import qualified Text.PrettyPrint as PP

import           Data.HList.ContainsType

import           UI.Butcher.Monadic.Internal.Types
import           UI.Butcher.Monadic.Internal.Core
import           UI.Butcher.Monadic.Pretty
import           UI.Butcher.Monadic.Param

import           System.IO



-- | Adds a proper full help command. To obtain the 'CommandDesc' value, see
-- 'UI.Butcher.Monadic.cmdRunParserWithHelpDesc' or
-- 'UI.Butcher.Monadic.IO.mainFromCmdParserWithHelpDesc'.
addHelpCommand :: Applicative f => CommandDesc a -> CmdParser f (IO ()) ()
addHelpCommand desc = addCmd "help" $ do
  rest <- addParamRestOfInput "SUBCOMMAND(s)" mempty
  addCmdImpl $ do
    let restWords = List.words rest
    let
      descent :: [String] -> CommandDesc a -> CommandDesc a
      descent [] curDesc = curDesc
      descent (w:wr) curDesc =
        case
            List.lookup (Just w) $ Data.Foldable.toList $ _cmd_children curDesc
          of
            Nothing    -> curDesc
            Just child -> descent wr child
    print $ ppHelpShallow $ descent restWords desc

-- | Adds a help command that prints help for the command currently in context.
--
-- This version does _not_ include further childcommands, i.e. "help foo" will
-- not print the help for subcommand "foo".
--
-- This also yields slightly different output depending on if it is used
-- before or after adding other subcommands. In general 'addHelpCommand'
-- should be preferred.
addHelpCommandShallow :: Applicative f => CmdParser f (IO ()) ()
addHelpCommandShallow = addCmd "help" $ do
  desc <- peekCmdDesc
  _rest <- addParamRestOfInput "SUBCOMMAND(s)" mempty
  addCmdImpl $ do
    let parentDesc = maybe undefined snd (_cmd_mParent desc)
    print $ ppHelpShallow $ parentDesc

-- | Prints the raw CommandDesc structure.
addButcherDebugCommand :: Applicative f => CmdParser f (IO ()) ()
addButcherDebugCommand = addCmd "butcherdebug" $ do
  desc <- peekCmdDesc
  addCmdImpl $ do
    print $ maybe undefined snd (_cmd_mParent desc)

