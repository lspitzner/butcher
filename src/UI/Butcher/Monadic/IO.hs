-- | Turn your CmdParser into an IO () to be used as your program @main@.
module UI.Butcher.Monadic.IO
  ( mainFromCmdParser
  , mainFromCmdParserWithHelpDesc
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



-- | Utility method that allows using a 'CmdParser' as your @main@ function:
--
-- > main = mainFromCmdParser $ do
-- >   addCmdImpl $ putStrLn "This is a fairly boring program."
--
-- Uses @System.Environment.getProgName@ as program name and
-- @System.Environment.getArgs@ as the input to be parsed. Prints some
-- appropriate messages if parsing fails or if the command has no
-- implementation; if all is well executes the \'out\' action (the IO ()).
mainFromCmdParser :: CmdParser Identity (IO ()) () -> IO ()
mainFromCmdParser cmd = do
  progName <- System.Environment.getProgName
  case checkCmdParser (Just progName) cmd of
    Left  e -> do
      putStrErrLn
        $ progName
        ++ ": internal error: failed sanity check for butcher main command parser!"
      putStrErrLn $ "(" ++ e ++ ")"
      putStrErrLn $ "aborting."
    Right _ -> do
      args <- System.Environment.getArgs
      case runCmdParser (Just progName) (InputArgs args) cmd of
        (desc, Left (ParsingError mess remaining)) -> do
          putStrErrLn
            $  progName
            ++ ": error parsing arguments: "
            ++ case mess of
                 []    -> ""
                 (m:_) -> m
          putStrErrLn $ case remaining of
            InputString ""  -> "at the end of input."
            InputString str -> case show str of
              s | length s < 42 -> "at: " ++ s ++ "."
              s                 -> "at: " ++ take 40 s ++ "..\"."
            InputArgs   []  -> "at the end of input"
            InputArgs   xs  -> case List.unwords $ show <$> xs of
              s | length s < 42 -> "at: " ++ s ++ "."
              s                 -> "at: " ++ take 40 s ++ "..\"."
          putStrErrLn $ "usage:"
          printErr $ ppUsage desc
        (desc, Right out                         ) -> case _cmd_out out of
          Nothing -> do
            putStrErrLn $ "usage:"
            printErr $ ppUsage desc
          Just a  -> a

-- | Same as mainFromCmdParser, but with one additional twist: You get access
-- to a knot-tied complete CommandDesc for this full command. Useful in
-- combination with 'UI.Butcher.Monadic.BuiltinCommands.addHelpCommand'
mainFromCmdParserWithHelpDesc
  :: (CommandDesc () -> CmdParser Identity (IO ()) ()) -> IO ()
mainFromCmdParserWithHelpDesc cmdF = do
  progName <- System.Environment.getProgName
  let (checkResult, fullDesc)
        -- knot-tying at its finest..
        = ( checkCmdParser (Just progName) (cmdF fullDesc)
          , either (const emptyCommandDesc) id $ checkResult
          )
  case checkResult of
    Left e -> do
      putStrErrLn $ progName ++ ": internal error: failed sanity check for butcher main command parser!"
      putStrErrLn $ "(" ++ e ++ ")"
      putStrErrLn $ "aborting."
    Right _ -> do
      args <- System.Environment.getArgs
      case runCmdParser (Just progName) (InputArgs args) (cmdF fullDesc) of
        (desc, Left (ParsingError mess remaining)) -> do
          putStrErrLn $ progName ++ ": error parsing arguments: " ++ head mess
          putStrErrLn $ case remaining of
            InputString "" -> "at the end of input."
            InputString str -> case show str of
              s | length s < 42 -> "at: " ++ s ++ "."
              s -> "at: " ++ take 40 s ++ "..\"."
            InputArgs [] -> "at the end of input"
            InputArgs xs -> case List.unwords $ show <$> xs of
              s | length s < 42 -> "at: " ++ s ++ "."
              s -> "at: " ++ take 40 s ++ "..\"."
          putStrErrLn $ "usage:"
          printErr $ ppUsage desc
        (desc, Right out) -> case _cmd_out out of
          Nothing -> do
            putStrErrLn $ "usage:"
            printErr $ ppUsage desc
          Just a -> a

putStrErrLn :: String -> IO ()
putStrErrLn s = hPutStrLn stderr s

printErr :: Show a => a -> IO ()
printErr = putStrErrLn . show
