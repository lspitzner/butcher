module UI.Butcher.Monadic
  ( module Export
  , cmds
  -- , sample
  -- , test
  , test2
  , test3
  )
where



#include "qprelude/bundle-gamma.inc"

import qualified Text.PrettyPrint as PP

import UI.Butcher.Monadic.Types as Export
import UI.Butcher.Monadic.Core as Export
import UI.Butcher.Monadic.Flag as Export
import UI.Butcher.Monadic.Param as Export
import UI.Butcher.Monadic.Pretty as Export
import UI.Butcher.Monadic.IO as Export

-- import qualified Options.Applicative as OPA


cmds :: CmdParser Identity (IO ()) ()
cmds = do
  addCmd "echo" $ do
    addCmdHelpStr "print its parameter to output"
    str <- addReadParam "STRING" (paramHelpStr "the string to print")
    addCmdImpl $ do
      putStrLn str
  addCmd "hello" $ do
    addCmdHelpStr "greet the user"
    reorderStart
    short <- addSimpleBoolFlag "" ["short"] mempty
    name <- addReadParam "NAME" (paramHelpStr "your name, so you can be greeted properly"
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

test2 :: IO ()
test2 = case cmdCheckParser (Just "butcher") cmds of
  Left e -> putStrLn $ "LEFT: " ++ e
  Right desc -> do
    print $ ppUsage desc
    print $ maybe undefined id $ ppUsageAt ["hello"] desc

test3 :: String -> IO ()
test3 s = case cmdRunParser (Just "butcher") (InputString s) cmds of
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
