## definitions

~~~~.hs
exampleCmdParser :: CmdParser Identity Int ()
exampleCmdParser = do
  addCmd "foo" $ addCmdImpl 42
  addCmd "bar" $ addCmdImpl 99
  addCmdImpl 0

fooBarParser :: String -> Either ParsingError (CommandDesc Int)
fooBarParser str = result
  where
    (_desc, result) =
      runCmdParser (Just "example") (InputString str) exampleCmdParser
~~~~

## Behaviour of fooBarParser:

~~~~
fooBarParser ""    ~> Right 0
foobarParser "foo" ~> Right 42
foobarParser "bar" ~> Right 99
fooBarParser _     ~> Left someParsingError
~~~~
