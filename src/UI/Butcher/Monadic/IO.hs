module UI.Butcher.Monadic.IO
  ( mainFromCmdParser
  , addHelpCommand
  , addButcherDebugCommand
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS

import qualified Text.PrettyPrint as PP

import           Data.HList.ContainsType

import           UI.Butcher.Monadic.Types
import           UI.Butcher.Monadic.Core
import           UI.Butcher.Monadic.Pretty

import           System.IO



mainFromCmdParser :: CmdParser Identity (IO ()) () -> IO ()
mainFromCmdParser cmd = do
  progName <- System.Environment.getProgName
  case cmdCheckParser (Just progName) cmd of
    Left e -> do
      putStrErrLn $ progName ++ ": internal error: failed sanity check for butcher main command parser!"
      putStrErrLn $ "(" ++ e ++ ")"
      putStrErrLn $ "aborting."
    Right _ -> do
      args <- System.Environment.getArgs
      case cmdRunParser (Just progName) (InputArgs args) cmd of
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

addHelpCommand :: Applicative f => CmdParser f (IO ()) ()
addHelpCommand = addCmd "help" $ do
  desc <- peekCmdDesc
  addCmdImpl $ do
    print $ ppHelpShallow $ maybe undefined snd (_cmd_mParent desc)

addButcherDebugCommand :: Applicative f => CmdParser f (IO ()) ()
addButcherDebugCommand = addCmd "butcherdebug" $ do
  desc <- peekCmdDesc
  addCmdImpl $ do
    print $ maybe undefined snd (_cmd_mParent desc)

putStrErrLn :: String -> IO ()
putStrErrLn s = hPutStrLn stderr s

printErr :: Show a => a -> IO ()
printErr = putStrErrLn . show
