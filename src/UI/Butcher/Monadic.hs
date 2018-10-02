-- | Reexports of everything that is exposed in the submodules.
module UI.Butcher.Monadic
  ( -- * Types
    Input (..)
  , CmdParser
  , ParsingError (..)
  , CommandDesc(_cmd_out)
  , cmd_out
  , -- * Run or Check CmdParsers
    runCmdParserSimple
  , runCmdParser
  , runCmdParserExt
  , runCmdParserA
  , runCmdParserAExt
  , runCmdParserWithHelpDesc
  , checkCmdParser
  , -- * Building CmdParsers
    module UI.Butcher.Monadic.Command
    -- * PrettyPrinting CommandDescs (usage/help)
  , module UI.Butcher.Monadic.Pretty
    -- * Wrapper around System.Environment.getArgs
  , module UI.Butcher.Monadic.IO
    -- * Utilities for interactive feedback of commandlines (completions etc.)
  , module UI.Butcher.Monadic.Interactive
  -- , cmds
  -- , sample
  -- , test
  -- , test2
  -- , test3
    -- * Builtin commands
  , addHelpCommand
  , addHelpCommand2
  , addHelpCommandWith
  , addButcherDebugCommand
  , addShellCompletionCommand
  , addShellCompletionCommand'
    -- * Advanced usage
  , mapOut
  , emptyCommandDesc
  , Visibility (..)
  )
where



#include "prelude.inc"

import UI.Butcher.Monadic.Types
import UI.Butcher.Monadic.Internal.Types
import UI.Butcher.Monadic.Command
import UI.Butcher.Monadic.BuiltinCommands
import UI.Butcher.Monadic.Internal.Core
import UI.Butcher.Monadic.Pretty
import UI.Butcher.Monadic.IO
import UI.Butcher.Monadic.Interactive

import qualified Text.PrettyPrint as PP



#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif



-- | Like 'runCmdParser', but with one additional twist: You get access
-- to a knot-tied complete CommandDesc for this full command. Useful in
-- combination with 'UI.Butcher.Monadic.BuiltinCommands.addHelpCommand'.
--
-- Note that the @CommandDesc ()@ in the output is _not_ the same value as the
-- parameter passed to the parser function: The output value contains a more
-- "shallow" description. This is more efficient for complex CmdParsers when
-- used interactively, because non-relevant parts of the CmdParser are not
-- traversed unless the parser function argument is forced.
runCmdParserWithHelpDesc
  :: Maybe String -- ^ program name to be used for the top-level @CommandDesc@
  -> Input -- ^ input to be processed
  -> (CommandDesc () -> CmdParser Identity out ()) -- ^ parser to use
  -> (CommandDesc (), Either ParsingError (CommandDesc out))
runCmdParserWithHelpDesc mProgName input cmdF =
  let (checkResult, fullDesc)
        -- knot-tying at its finest..
        = ( checkCmdParser mProgName (cmdF fullDesc)
          , either (const emptyCommandDesc) id $ checkResult
          )
  in runCmdParser mProgName input (cmdF fullDesc)


-- | Wrapper around 'runCmdParser' for very simple usage: Accept a @String@
-- input and return only the output from the parser, or a plain error string
-- on failure.
runCmdParserSimple :: String -> CmdParser Identity out () -> Either String out
runCmdParserSimple s p = case snd $ runCmdParser Nothing (InputString s) p of
  Left e -> Left $ parsingErrorString e
  Right desc ->
    maybe (Left "command has no implementation") Right $ _cmd_out desc


--------------------------------------
-- all below is for testing purposes
--------------------------------------


_cmds :: CmdParser Identity (IO ()) ()
_cmds = do
  addCmd "echo" $ do
    addCmdHelpStr "print its parameter to output"
    str <- addParamRead "STRING" (paramHelpStr "the string to print")
    addCmdImpl $ do
      putStrLn str
  addCmd "hello" $ do
    addCmdHelpStr "greet the user"
    reorderStart
    short <- addSimpleBoolFlag "" ["short"] mempty
    name <- addParamRead "NAME" (paramHelpStr "your name, so you can be greeted properly"
                              <> paramDefault "user")
    reorderStop
    addCmdImpl $ do
      if short
        then putStrLn $ "hi, " ++ name ++ "!"
        else putStrLn $ "hello, " ++ name ++ ", welcome from butcher!"
  addCmd "foo" $ do
    addCmdHelpStr "foo"
    desc <- peekCmdDesc
    addCmdImpl $ do
      putStrLn "foo"
      print $ ppHelpShallow desc
  addCmd "help" $ do
    desc <- peekCmdDesc
    addCmdImpl $ do
      print $ ppHelpShallow $ maybe undefined snd (_cmd_mParent desc)

data Sample = Sample
  { _hello :: Int
  , _s1   :: String
  , _s2   :: String
  , _quiet :: Bool
  }
  deriving Show

-- sample :: OPA.Parser Sample
-- sample = Sample
--      <$> OPA.option OPA.auto
--          ( OPA.long "hello"
--         <> OPA.metavar "TARGET"
--         <> OPA.help "Target for the greeting" )
--      <*> OPA.strArgument (OPA.metavar "S1")
--      <*> OPA.strArgument (OPA.metavar "S2")
--      <*> OPA.switch
--          ( OPA.long "quiet"
--         <> OPA.help "Whether to be quiet" )
-- 
-- test :: String -> OPA.ParserResult Sample
-- test s = OPA.execParserPure OPA.defaultPrefs (OPA.ParserInfo sample True mempty mempty mempty (-13) True) (List.words s)

_test2 :: IO ()
_test2 = case checkCmdParser (Just "butcher") _cmds of
  Left e -> putStrLn $ "LEFT: " ++ e
  Right desc -> do
    print $ ppUsage desc
    print $ maybe undefined id $ ppUsageAt ["hello"] desc

_test3 :: String -> IO ()
_test3 s = case runCmdParser (Just "butcher") (InputString s) _cmds of
  (desc, Left e) -> do
    print e
    print $ ppHelpShallow desc
    _cmd_mParent desc `forM_` \(_, d) -> do
      print $ ppUsage d
  (desc, Right out) -> do
    case _cmd_out out of
      Nothing -> do
        putStrLn "command is missing implementation!"
        print $ ppHelpShallow desc
      Just f -> f
