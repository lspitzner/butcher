## program

~~~~.hs
data Out = Abort | Continue (IO ())

main = do
  putStrLn "example interactive commandline program."
  loop
 where
  cmdParser :: CmdParser Identity Out ()
  cmdParser = do
    addCmd "exit" $ addCmdImpl Abort
    addCmd "greeting" $ addCmdImpl $ Continue $ putStrLn "hi!"
  loop = do
    putStr "example> "
    hFlush stdout
    line <- getLine
    case cmdRunParser Nothing (InputString line) cmdParser of
      (_, Left err) -> do
        print err
        loop
      (_, Right desc) -> case _cmd_out desc of
        Nothing -> do
          putStrLn "Usage: "
          print $ ppUsage desc
          loop
        Just Abort -> return ()
        Just (Continue action) -> do
          action
          loop
~~~~

## sample session:

~~~~
bash> ./example<enter>
example interactive commandline program.
example> <enter>
Usage: 
exit | greeting
example> greeting<enter>
hi!
example> exit<enter>
bash> 
~~~~
