## CmdParser definition

~~~~.hs
main = mainFromCmdParserWithHelpDesc $ \helpDesc -> do

  addCmdSynopsis "a simple butcher example program"
  addCmdHelpStr "a very long help document"

  addCmd "version" $ do
    porcelain <- addSimpleBoolFlag "" ["porcelain"]
      (flagHelpStr "print nothing but the numeric version")
    addCmdHelpStr "prints the version of this program"
    addCmdImpl $ putStrLn $ if porcelain
      then "1.0"
      else "example, version 1.0"

  addCmd "help" $ addCmdImpl $ print $ ppHelpShallow helpDesc

  short <- addSimpleBoolFlag "" ["short"]
    (flagHelpStr "make the greeting short")
  name <- addStringParam "NAME"
    (paramHelpStr "your name, so you can be greeted properly")

  addCmdImpl $ do
    if short
      then putStrLn $ "hi, " ++ name ++ "!"
      else putStrLn $ "hello, " ++ name ++ ", welcome from butcher!"
~~~~

## Program behaviour (executable is named `example`):

~~~~
> ./example
example: error parsing arguments: could not parse NAME
at the end of input
usage:
example [--short] NAME [version | help]
~~~~

---

~~~~
> ./example help
NAME

  example - a simple butcher example program

USAGE

  example [--short] NAME [version | help]

DESCRIPTION

  a very long help document

ARGUMENTS

  --short             make the greeting short
  NAME                your name, so you can be greeted properly
~~~~

---

~~~~
> ./example garfield
hello, garfield, welcome from butcher!
~~~~

---

~~~~
> ./example --short garfield
hi, garfield!
~~~~

---

~~~~
> ./example version --porcelain
1.0
~~~~
