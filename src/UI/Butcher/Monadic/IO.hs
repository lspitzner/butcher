module UI.Butcher.Monadic.IO
  ( mainFromCmdParser
  , addHelpCommand
  , addButcherDebugCommand
  )
where



#include "qprelude/bundle-gamma.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS
import           Data.Unique (Unique)
import qualified System.Unsafe as Unsafe

import qualified Control.Lens.TH as LensTH
import qualified Control.Lens as Lens
import           Control.Lens ( (.=), (%=), (%~), (.~) )

import qualified Text.PrettyPrint as PP

import           Data.HList.ContainsType

import           Data.Dynamic

import           UI.Butcher.Monadic.Types
import           UI.Butcher.Monadic.Core
import           UI.Butcher.Monadic.Pretty



mainFromCmdParser :: CmdParser Identity (IO ()) () -> IO ()
mainFromCmdParser cmd = do
  progName <- System.Environment.getProgName
  case cmdCheckParser (Just progName) cmd of
    Left e -> do
      putStrLn $ progName ++ ": internal error: failed sanity check for butcher main command parser!"
      putStrLn $ "(" ++ e ++ ")"
      putStrLn $ "aborting."
    Right _ -> do
      args <- System.Environment.getArgs
      case cmdRunParser (Just progName) (InputArgs args) cmd of
        (desc, Left (ParsingError mess remaining)) -> do
          putStrLn $ progName ++ ": error parsing arguments: " ++ head mess
          putStrLn $ case remaining of
            InputString "" -> "at the end of input."
            InputString str -> case show str of
              s | length s < 42 -> "at: " ++ s ++ "."
              s -> "at: " ++ take 40 s ++ "..\"."
            InputArgs [] -> "at the end of input"
            InputArgs xs -> case List.unwords $ show <$> xs of
              s | length s < 42 -> "at: " ++ s ++ "."
              s -> "at: " ++ take 40 s ++ "..\"."
          putStrLn $ "usage:"
          print $ ppUsage desc
        (desc, Right out) -> case _cmd_out out of
          Nothing -> do
            putStrLn $ "usage:"
            print $ ppUsage desc
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
