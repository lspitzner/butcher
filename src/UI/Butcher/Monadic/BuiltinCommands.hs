-- | Some CmdParser actions that add predefined commands.
module UI.Butcher.Monadic.BuiltinCommands
  ( addHelpCommand
  , addHelpCommand2
  , addHelpCommandWith
  , addHelpCommandShallow
  , addButcherDebugCommand
  , addShellCompletionCommand
  , addShellCompletionCommand'
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
import           UI.Butcher.Monadic.Interactive

import           System.IO



-- | Adds a proper full help command. To obtain the 'CommandDesc' value, see
-- 'UI.Butcher.Monadic.cmdRunParserWithHelpDesc' or
-- 'UI.Butcher.Monadic.IO.mainFromCmdParserWithHelpDesc'.
--
-- > addHelpCommand = addHelpCommandWith
-- >   (pure . PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0 } . ppHelpShallow)
addHelpCommand :: Applicative f => CommandDesc a -> CmdParser f (IO ()) ()
addHelpCommand = addHelpCommandWith
  (pure . PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0 } . ppHelpShallow)

-- | Adds a proper full help command. In contrast to 'addHelpCommand',
-- this version is a bit more verbose about available subcommands as it
-- includes their synopses.
--
-- To obtain the 'CommandDesc' value, see
-- 'UI.Butcher.Monadic.cmdRunParserWithHelpDesc' or
-- 'UI.Butcher.Monadic.IO.mainFromCmdParserWithHelpDesc'.
--
-- > addHelpCommand2 = addHelpCommandWith
-- >   (pure . PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0 } . ppHelpDepthOne)
addHelpCommand2 :: Applicative f => CommandDesc a -> CmdParser f (IO ()) ()
addHelpCommand2 = addHelpCommandWith
  (pure . PP.renderStyle PP.style { PP.ribbonsPerLine = 1.0 } . ppHelpDepthOne)

-- | Adds a proper full help command, using the specified function to turn
-- the relevant subcommand's 'CommandDesc' into a String.
addHelpCommandWith
  :: Applicative f
  => (CommandDesc a -> IO String)
  -> CommandDesc a
  -> CmdParser f (IO ()) ()
addHelpCommandWith f desc = addCmd "help" $ do
  addCmdSynopsis "print help about this command"
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
    s <- f $ descent restWords desc
    putStrLn s

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

-- | Adds the "completion" command and several subcommands.
--
-- This command can be used in the following manner:
--
-- > $ source <(foo completion bash-script foo)
addShellCompletionCommand
  :: CmdParser Identity (IO ()) () -> CmdParser Identity (IO ()) ()
addShellCompletionCommand mainCmdParser = do
  addCmdHidden "completion" $ do
    addCmdSynopsis "utilites to enable bash-completion"
    addCmd "bash-script" $ do
      addCmdSynopsis "generate a bash script for completion functionality"
      exeName <- addParamString "EXENAME" mempty
      addCmdImpl $ do
        putStr $ completionScriptBash exeName
    addCmd "bash-gen" $ do
      addCmdSynopsis
        "generate possible completions for given input arguments"
      rest <- addParamRestOfInputRaw "REALCOMMAND" mempty
      addCmdImpl $ do
        let (cdesc, remaining, _result) =
              runCmdParserExt Nothing rest mainCmdParser
        let
          compls = shellCompletionWords (inputString rest)
                                        cdesc
                                        (inputString remaining)
        let lastWord =
              reverse $ takeWhile (not . Char.isSpace) $ reverse $ inputString
                rest
        putStrLn $ List.unlines $ compls <&> \case
          CompletionString s  -> s
          CompletionFile      -> "$(compgen -f -- " ++ lastWord ++ ")"
          CompletionDirectory -> "$(compgen -d -- " ++ lastWord ++ ")"
 where
  inputString (InputString s ) = s
  inputString (InputArgs   as) = List.unwords as

-- | Adds the "completion" command and several subcommands
--
-- This command can be used in the following manner:
--
-- > $ source <(foo completion bash-script foo)
addShellCompletionCommand'
  :: (CommandDesc out -> CmdParser Identity (IO ()) ())
  -> CmdParser Identity (IO ()) ()
addShellCompletionCommand' f = addShellCompletionCommand (f emptyCommandDesc)

completionScriptBash :: String -> String
completionScriptBash exeName =
  List.unlines
    $ [ "function _" ++ exeName ++ "()"
      , "{"
      , "  local IFS=$'\\n'"
      , "  COMPREPLY=()"
      , "  local result=$("
      ++ exeName
      ++ " completion bash-gen \"${COMP_WORDS[@]:1}\")"
      , "  for r in ${result[@]}; do"
      , "    local IFS=$'\\n '"
      , "    for s in $(eval echo ${r}); do"
      , "      COMPREPLY+=(${s})"
      , "    done"
      , "  done"
      , "}"
      , "complete -F _" ++ exeName ++ " " ++ exeName
      ]

