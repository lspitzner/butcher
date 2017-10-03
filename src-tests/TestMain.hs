module Main where



#include "prelude.inc"

import Test.Hspec

-- import NeatInterpolation

import UI.Butcher.Monadic
import UI.Butcher.Monadic.Types



main :: IO ()
main = hspec $ tests

tests :: Spec
tests = do
  describe "checkTests" checkTests
  describe "simpleParseTest" simpleParseTest
  describe "simpleRunTest" simpleRunTest


checkTests :: Spec
checkTests = do
  before_ pending $ it "check001" $ True `shouldBe` True


simpleParseTest :: Spec
simpleParseTest = do
  it "failed parse 001" $ runCmdParser Nothing (InputString "foo") testCmd1
         `shouldSatisfy` Data.Either.Combinators.isLeft . snd
  it "toplevel" $ (testParse testCmd1 "" >>= _cmd_out)
                  `shouldSatisfy` Maybe.isNothing
  it "hasImpl 001" $ (testParse testCmd1 "abc" >>= _cmd_out)
                  `shouldSatisfy` Maybe.isJust
  it "hasImpl 002" $ (testParse testCmd1 "def" >>= _cmd_out)
                  `shouldSatisfy` Maybe.isJust


simpleRunTest :: Spec
simpleRunTest = do
  it "failed run" $ testRun testCmd1 "" `shouldBe` Right Nothing
  describe "no reordering" $ do
    it "cmd 1" $ testRun testCmd1 "abc" `shouldBe` Right (Just 100)
    it "cmd 2" $ testRun testCmd1 "def" `shouldBe` Right (Just 200)
    it "flag 1" $ testRun testCmd1 "abc -f" `shouldBe` Right (Just 101)
    it "flag 2" $ testRun testCmd1 "abc --flong" `shouldBe` Right (Just 101)
    it "flag 3" $ testRun testCmd1 "abc -f -f" `shouldBe` Right (Just 101)
    it "flag 4" $ testRun testCmd1 "abc -f -g" `shouldBe` Right (Just 103)
    it "flag 5" $ testRun testCmd1 "abc -f -g -f" `shouldSatisfy` Data.Either.Combinators.isLeft -- no reordering
    it "flag 6" $ testRun testCmd1 "abc -g -f" `shouldSatisfy` Data.Either.Combinators.isLeft -- no reordering
    it "flag 7" $ testRun testCmd1 "abc -g -g" `shouldBe` Right (Just 102)
  describe "with reordering" $ do
    it "cmd 1" $ testRun testCmd2 "abc" `shouldBe` Right (Just 100)
    it "cmd 2" $ testRun testCmd2 "def" `shouldBe` Right (Just 200)
    it "flag 1" $ testRun testCmd2 "abc -f" `shouldBe` Right (Just 101)
    it "flag 2" $ testRun testCmd2 "abc --flong" `shouldBe` Right (Just 101)
    it "flag 3" $ testRun testCmd2 "abc -f -f" `shouldBe` Right (Just 101)
    it "flag 4" $ testRun testCmd2 "abc -f -g" `shouldBe` Right (Just 103)
    it "flag 5" $ testRun testCmd2 "abc -f -g -f" `shouldBe` Right (Just 103)
    it "flag 6" $ testRun testCmd2 "abc -g -f" `shouldBe` Right (Just 103)
    it "flag 7" $ testRun testCmd2 "abc -g -g" `shouldBe` Right (Just 102)
  describe "with action" $ do
    it "flag 1" $ testRunA testCmd3 "abc" `shouldBe` Right 0
    it "flag 2" $ testRunA testCmd3 "abc -f" `shouldBe` Right 1
    it "flag 3" $ testRunA testCmd3 "abc -g" `shouldBe` Right 2
    it "flag 4" $ testRunA testCmd3 "abc -f -g" `shouldBe` Right 3
    it "flag 5" $ testRunA testCmd3 "abc -g -f" `shouldBe` Right 3
  describe "separated children" $ do
    it "case 1" $ testRun testCmd4 "a aa" `shouldBe` Right (Just 1)
    it "case 2" $ testRun testCmd4 "a ab" `shouldBe` Right (Just 2)
    it "case 3" $ testRun testCmd4 "b ba" `shouldBe` Right (Just 3)
    it "case 4" $ testRun testCmd4 "b bb" `shouldBe` Right (Just 4)
    it "doc" $ show (ppHelpShallow (getDoc "" testCmd4)) `shouldBe`
      List.unlines
        [ "NAME"
        , ""
        , "  test"
        , ""
        , "USAGE"
        , ""
        , "  test a | b"
        ]
    it "doc" $ show (ppHelpShallow (getDoc "a" testCmd4)) `shouldBe`
      List.unlines
        [ "NAME"
        , ""
        , "  test a"
        , ""
        , "USAGE"
        , ""
        , "  test a aa | ab"
        ]
  describe "read flags" $ do
    it "flag 1" $ testRun testCmd5 "abc" `shouldBe` Right (Just 10)
    it "flag 2" $ testRun testCmd5 "abc -f 2" `shouldBe` Right (Just 2)
    it "flag 3" $ testRun testCmd5 "abc --flag 3" `shouldBe` Right (Just 3)
    it "flag 4" $ testRun testCmd5 "abc -f=4" `shouldBe` Right (Just 4)
    it "flag 5" $ testRun testCmd5 "abc --flag=5" `shouldBe` Right (Just 5)
    it "flag 6" $ testRun testCmd5 "abc -f" `shouldSatisfy` Data.Either.isLeft
    it "flag 6" $ testRun testCmd5 "abc -flag 0" `shouldSatisfy` Data.Either.isLeft
    it "flag 6" $ testRun testCmd5 "abc --f 0" `shouldSatisfy` Data.Either.isLeft
  describe "addParamStrings" $ do
    it "case 1" $ testRun' testCmd6 "" `shouldBe` Right (Just ([], 0))
    it "case 2" $ testRun' testCmd6 "-f" `shouldBe` Right (Just ([], 1))
    it "case 3" $ testRun' testCmd6 "abc" `shouldBe` Right (Just (["abc"], 0))
    it "case 4" $ testRun' testCmd6 "abc def" `shouldBe` Right (Just (["abc", "def"], 0))
    it "case 5" $ testRun' testCmd6 "-g abc def" `shouldBe` Right (Just (["abc", "def"], 2))
    it "case 6" $ testRun' testCmd6 "-f -g def" `shouldBe` Right (Just (["def"], 3))
  describe "addParamNoFlagStrings" $ do
    it "case 1" $ testRun' testCmd7 "" `shouldBe` Right (Just ([], 0))
    it "case 2" $ testRun' testCmd7 "-f" `shouldBe` Right (Just ([], 1))
    it "case 3" $ testRun' testCmd7 "abc" `shouldBe` Right (Just (["abc"], 0))
    it "case 4" $ testRun' testCmd7 "abc -f" `shouldBe` Right (Just (["abc"], 1))
    it "case 5" $ testRun' testCmd7 "-g abc -f" `shouldBe` Right (Just (["abc"], 3))
    it "case 6" $ testRun' testCmd7 "abc -g def" `shouldBe` Right (Just (["abc", "def"], 2))



testCmd1 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd1 = do
  addCmd "abc" $ do
    f <- addSimpleBoolFlag "f" ["flong"] mempty
    g <- addSimpleBoolFlag "g" ["glong"] mempty
    addCmdImpl $ do
      when f $ WriterS.tell 1
      when g $ WriterS.tell 2
      WriterS.tell 100
  addCmd "def" $ do
    addCmdImpl $ do
      WriterS.tell 200

testCmd2 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd2 = do
  addCmd "abc" $ do
    reorderStart
    f <- addSimpleBoolFlag "f" ["flong"] mempty
    g <- addSimpleBoolFlag "g" ["glong"] mempty
    reorderStop
    addCmdImpl $ do
      when f $ WriterS.tell 1
      when g $ WriterS.tell 2
      WriterS.tell 100
  addCmd "def" $ do
    addCmdImpl $ do
      WriterS.tell 200

testCmd3 :: CmdParser (StateS.State Int) () ()
testCmd3 = do
  addCmd "abc" $ do
    reorderStart
    addSimpleFlagA "f" ["flong"] mempty (StateS.modify (+1))
    addSimpleFlagA "g" ["glong"] mempty (StateS.modify (+2))
    reorderStop
    addCmdImpl ()
  addCmd "def" $ do
    addCmdImpl ()

testCmd4 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd4 = do
  addCmd "a" $ do
    addCmd "aa" $ do
      addCmdImpl $ WriterS.tell 1
  addCmd "b" $ do
    addCmd "bb" $ do
      addCmdImpl $ WriterS.tell 4
  addCmd "a" $ do
    addCmd "ab" $ do
      addCmdImpl $ WriterS.tell 2
  addCmd "b" $ do
    addCmd "ba" $ do
      addCmdImpl $ WriterS.tell 3

testCmd5 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd5 = do
  addCmd "abc" $ do
    x <- addFlagReadParam "f" ["flag"] "flag" (flagDefault (10::Int))
    addCmdImpl $ WriterS.tell (Sum x)

testCmd6 :: CmdParser Identity (WriterS.Writer (Sum Int) [String]) ()
testCmd6 = do
  f <- addSimpleBoolFlag "f" ["flong"] mempty
  g <- addSimpleBoolFlag "g" ["glong"] mempty
  args <- addParamStrings "ARGS" mempty
  addCmdImpl $ do
    when f $ WriterS.tell 1
    when g $ WriterS.tell 2
    pure args

testCmd7 :: CmdParser Identity (WriterS.Writer (Sum Int) [String]) ()
testCmd7 = do
  reorderStart
  f <- addSimpleBoolFlag "f" ["flong"] mempty
  g <- addSimpleBoolFlag "g" ["glong"] mempty
  args <- addParamNoFlagStrings "ARGS" mempty
  reorderStop
  addCmdImpl $ do
    when f $ WriterS.tell 1
    when g $ WriterS.tell 2
    pure args


testParse :: CmdParser Identity out () -> String -> Maybe (CommandDesc out)
testParse cmd s = either (const Nothing) Just
                $ snd
                $ runCmdParser Nothing (InputString s) cmd

testRun :: CmdParser Identity (WriterS.Writer (Sum Int) ()) () -> String -> Either ParsingError (Maybe Int)
testRun cmd s = fmap (fmap (getSum . WriterS.execWriter) . _cmd_out)
              $ snd
              $ runCmdParser Nothing (InputString s) cmd

testRun' :: CmdParser Identity (WriterS.Writer (Sum Int) a) () -> String -> Either ParsingError (Maybe (a, Int))
testRun' cmd s =
  fmap (fmap (fmap getSum . WriterS.runWriter) . _cmd_out) $ snd $ runCmdParser
    Nothing
    (InputString s)
    cmd

testRunA :: CmdParser (StateS.State Int) () () -> String -> Either ParsingError Int
testRunA cmd str = (\((_, e), s) -> e $> s)
                 $ flip StateS.runState (0::Int)
                 $ runCmdParserA Nothing (InputString str) cmd

getDoc :: String -> CmdParser Identity out () -> CommandDesc ()
getDoc s = fst . runCmdParser (Just "test") (InputString s)
