{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf #-}

module UI.CmdParse.Monadic
  ( main
  , cmdActionPartial
  , cmdAction
  , cmd_run
  , CmdBuilder
  , addFlag
  , addCmd
  , addParam
  , help
  , impl
  , def
  , flagAsBool
  , cmdGetPartial
  , ppCommand
  , ppCommandShort
  , ppCommandShortHelp
    -- re-exports:
  , Command(..)
  , Flag(..)
  , Param(..)
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

import           UI.CmdParse.Monadic.Types

import           Data.HList.ContainsType



-- general-purpose helpers
----------------------------

mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = mGet >>= mSet . f

-- sadly, you need a degree in type inference to know when we can use
-- these operators and when it must be avoided due to type ambiguities
-- arising around s in the signatures below. That's the price of not having
-- the functional dependency in MonadMulti*T.

(.=+) :: MonadMultiState s m
      => Lens.ASetter s s a b -> b -> m ()
l .=+ b = mModify $ l .~ b

(%=+) :: MonadMultiState s m
      => Lens.ASetter s s a b -> (a -> b) -> m ()
l %=+ f = mModify (l %~ f)

-- inflateStateProxy :: (Monad m, ContainsType s ss)
--                   => p s -> StateS.StateT s m a -> MultiRWSS.MultiRWST r w ss m a
-- inflateStateProxy _ = MultiRWSS.inflateState

-- actual CmdBuilder stuff
----------------------------

instance IsHelpBuilder (CmdBuilder out) where
  help s = liftF $ CmdBuilderHelp s ()

instance IsHelpBuilder (ParamBuilder p) where
  help s = liftF $ ParamBuilderHelp s ()

instance IsHelpBuilder FlagBuilder where
  help s = liftF $ FlagBuilderHelp s ()

addCmd :: String -> CmdBuilder out () -> CmdBuilder out ()
addCmd s m = liftF $ CmdBuilderChild s m ()

addParam :: (Show p, IsParam p) => String -> ParamBuilder p () -> CmdBuilder out p
addParam s m = liftF $ CmdBuilderParam s m id

{-# NOINLINE addFlag #-}
addFlag :: String -> [String] -> FlagBuilder a -> CmdBuilder out [a]
addFlag shorts longs m = Unsafe.performIO $ do
  unique <- Data.Unique.newUnique
  return $ liftF $ CmdBuilderFlag unique shorts longs m id

impl :: a -> CmdBuilder a ()
impl x = liftF $ CmdBuilderRun x

def :: p -> ParamBuilder p ()
def d = liftF $ ParamBuilderDef d ()

-- | Does some limited "static" testing on a CmdBuilder.
--   "static" as in: it does not read any actual input.
--   Mostly checks that certain things are not defined multiple times,
--   e.g. help annotations.
cmdCheckNonStatic :: CmdBuilder out () -> Maybe String
cmdCheckNonStatic cmdBuilder = join
                             $ Data.Either.Combinators.leftToMaybe
                             $ flip StateS.evalState emptyCommand
                             $ runEitherT
                             $ iterM iterFunc cmdBuilder
  where
    iterFunc :: CmdBuilderF out
                        (EitherT (Maybe String)
                                 (StateS.State (Command out0)) a)
         -> EitherT (Maybe String)
                    (StateS.State (Command out0)) a
    iterFunc = \case
      CmdBuilderHelp h next -> do
        cmd <- State.Class.get
        case _cmd_help cmd of
          Nothing ->
            cmd_help .= Just h
          Just{} ->
            left $ Just $ "help is already defined when trying to add help \"" ++ h ++ "\""
        next
      CmdBuilderFlag funique _shorts _longs f next -> do
        case checkFlag funique f of -- yes, this is a mapM_.
          Nothing -> pure ()        -- but that does not help readability.
          err -> left $ err
        next []
      CmdBuilderParam _ p next -> do
        case checkParam p of
          Nothing -> pure ()
          err -> left $ err
        next $ paramStaticDef
      CmdBuilderChild _s c next -> do
        case cmdCheckNonStatic c of
          Nothing -> pure ()
          err -> left $ err
        next
      CmdBuilderRun _o ->
        left Nothing
    checkFlag :: Unique -> FlagBuilder b -> Maybe String
    checkFlag unique flagBuilder = join
                                 $ Data.Either.Combinators.leftToMaybe
                                 $ flip StateS.evalState (Flag unique "" [] Nothing [])
                                 $ runEitherT
                                 $ iterM iterFuncFlag flagBuilder
      where
        iterFuncFlag :: FlagBuilderF (EitherT (Maybe String) (StateS.State Flag) b)
                     -> EitherT (Maybe String) (StateS.State Flag) b
        iterFuncFlag = \case
          FlagBuilderHelp h next -> do
            param <- State.Class.get
            case _flag_help param of
              Nothing ->
                flag_help .= Just h
              Just{} ->
                left $ Just $ "help is already defined when trying to add help \"" ++ h ++ "\""
            next
          FlagBuilderParam _s p next -> do
            case checkParam p of
              Nothing -> pure ()
              err -> left $ err
            next $ paramStaticDef
    checkParam :: Show p => ParamBuilder p () -> Maybe String
    checkParam paramBuilder = join
                            $ Data.Either.Combinators.leftToMaybe
                            $ flip StateS.evalState (Param Nothing Nothing)
                            $ runEitherT
                            $ iterM iterFuncParam paramBuilder
      where
        iterFuncParam :: Show p
                      => ParamBuilderF p (EitherT (Maybe String) (StateS.State (Param p)) ())
                      -> EitherT (Maybe String) (StateS.State (Param p)) ()
        iterFuncParam = \case
          ParamBuilderHelp h next -> do
            param <- State.Class.get
            case _param_help param of
              Nothing ->
                param_help .= Just h
              Just{} ->
                left $ Just $ "help is already defined when trying to add help \"" ++ h ++ "\""
            next
          ParamBuilderDef d next -> do
            param <- State.Class.get
            case _param_def param of
              Nothing ->
                param_def .= Just d
              Just{} ->
                left $ Just $ "default is already defined when trying to add default \"" ++ show d ++ "\""
            next

cmdGetPartial :: forall out . String
              -> CmdBuilder out ()
              -> ( [String] -- errors
                 , String   -- remaining string
                 , Command out -- current result, as far as parsing was possible.
                               -- (!) take care not to run this command's action
                               -- if there are errors (!)
                 )
cmdGetPartial inputStr cmdBuilder
    = runIdentity
    $ MultiRWSS.runMultiRWSTNil
    $ (<&> captureFinal)
    $ MultiRWSS.withMultiWriterWA
    $ MultiRWSS.withMultiStateSA inputStr
    $ MultiRWSS.withMultiStateS emptyCommand
    $ processMain cmdBuilder
  where
    -- make sure that all input is processed; otherwise
    -- add an error.
    -- Does not use the writer because this method does some tuple
    -- shuffling.
    captureFinal :: ([String], (String, Command out))
                 -> ([String], String, Command out)
    captureFinal (errs, (s, cmd)) = (errs', s, cmd)
      where
        errs' = errs ++ if not $ all Char.isSpace s
          then ["could not parse input at " ++ s]
          else []

    -- main "interpreter" over the free monad. not implemented as an iteration
    -- because we switch to a different interpreter (and interpret the same
    -- stuff more than once) when processing flags.
    processMain :: CmdBuilder out ()
                -> MultiRWSS.MultiRWS '[] '[[String]] '[Command out, String] ()
    processMain = \case
      Pure x -> return x
      Free (CmdBuilderHelp h next) -> do
        cmd :: Command out <- mGet
        mSet $ cmd { _cmd_help = Just h }
        processMain next
      f@(Free (CmdBuilderFlag{})) -> do
        flagData <- MultiRWSS.withMultiWriterW $ -- WriterS.execWriterT $
                      iterM iterFlagGather f
        do
          cmd :: Command out <- mGet
          mSet $ cmd { _cmd_flags = _cmd_flags cmd ++ flagData }
        parsedFlag <- MultiRWSS.withMultiStateS (Map.empty :: FlagParsedMap)
                    $ parseFlags flagData
        (finalMap, fr) <- MultiRWSS.withMultiStateSA parsedFlag $ runParsedFlag f
        if Map.null finalMap
          then processMain fr
          else mTell ["internal error in application or colint library: inconsistent flag definitions."]
      Free (CmdBuilderParam s p next) -> do
        let param = processParam p
        cmd :: Command out <- mGet
        mSet $ cmd { _cmd_params = _cmd_params cmd ++ [ParamA s param] }
        str <- mGet
        x <- case (paramParse str, _param_def param) of
          (Nothing, Just x) -> do
            -- did not parse, use configured default value
            return $ x
          (Nothing, Nothing) -> do
            -- did not parse, no default value. add error, cont. with static default.
            mTell ["could not parse param at " ++ str]
            return paramStaticDef
          (Just (v, _, r), _) -> do
            -- parsed value; update the rest-string-to-parse, return value.
            mSet $ r
            return $ v
        processMain $ next $ x
      Free (CmdBuilderChild s c next) -> do
        dropSpaces
        str <- mGet
        let mRest = if
              | s == str -> Just ""
              | (s++" ") `isPrefixOf` str -> Just $ drop (length s + 1) str
              | otherwise -> Nothing
        case mRest of
          Nothing -> do
            cmd :: Command out <- mGet
            subCmd <- MultiRWSS.withMultiStateS emptyCommand
                    $ iterM processCmdShallow c
            mSet $ cmd { _cmd_children = _cmd_children cmd ++ [(s, subCmd)] }
            processMain next
          Just rest -> do
            old :: Command out <- mGet
            mSet $ rest
            mSet $ emptyCommand {
              _cmd_mParent = Just (old, s)
            }
            processMain c
      Free (CmdBuilderRun o) -> cmd_run .=+ Just o

    -- only captures some (i.e. roughly one layer) of the structure of
    -- the (remaining) builder, not parsing any input.
    processCmdShallow :: MonadMultiState (Command out) m
                      => CmdBuilderF out (m ())
                      -> m ()
    processCmdShallow = \case
      CmdBuilderHelp h next -> do
        cmd :: Command out <- mGet
        mSet $ cmd {
          _cmd_help = Just h
        }
        next
      CmdBuilderFlag _funique _shorts _longs _f next -> do
        next []
      CmdBuilderParam s p next -> do
        cmd :: Command out <- mGet
        mSet $ cmd {
          _cmd_params = _cmd_params cmd ++ [ParamA s $ processParam p]
        }
        next $ paramStaticDef
      CmdBuilderChild s _c next -> do
        cmd_children %=+ (++[(s, emptyCommand :: Command out)])
        next
      CmdBuilderRun _o ->
        return ()
    
    -- extract a list of flag declarations. return [], i.e. pretend that no
    -- flag matches while doing so.
    iterFlagGather :: CmdBuilderF out (MultiRWSS.MultiRWS r ([Flag]':wr) s ())
                   -> MultiRWSS.MultiRWS r ([Flag]':wr) s ()
    iterFlagGather = \case
      -- x | trace ("iterFlagGather: " ++ show (x $> ())) False -> error "laksjdlkja"
      CmdBuilderFlag funique shorts longs f next -> do
        let flag = processFlag funique shorts longs f
        mTell $ [flag]
        next []
      _ -> pure ()
    
    -- the second iteration (technically not an iterM, but close..) over flags:
    -- use the parsed flag map, so that the actual flag (values) are captured
    -- in this run.
    -- return the final CmdBuilder when a non-flag is encountered.
    runParsedFlag :: CmdBuilder out ()
                  -> MultiRWSS.MultiRWS '[] '[[String]] '[FlagParsedMap, Command out, String] (CmdBuilder out ())
    runParsedFlag = \case
      Free (CmdBuilderFlag funique _ _ f next) -> do
        m :: FlagParsedMap <- mGet
        let flagRawStrs = case Map.lookup funique m of
              Nothing -> []
              Just r -> r
        mSet $ Map.delete funique m
        runParsedFlag $ next $ reparseFlag f <$> flagRawStrs
      Pure x -> return $ return x
      f -> return f

    reparseFlag :: FlagBuilder b -> FlagParsedElement -> b
    reparseFlag = undefined -- TODO FIXME WHO LEFT THIS HERE

    parseFlags :: ( MonadMultiWriter [String] m
                  , MonadMultiState String m
                  , MonadMultiState FlagParsedMap m
                  )
               => [Flag]
               -> m ()
    parseFlags flags = do
      dropSpaces
      str <- mGet
      case str of
        ('-':'-':longRest) ->
          case getAlt $ mconcat $ flags <&> \f
                     -> mconcat $ _flag_long f <&> \l
                     -> let len = length l
                          in Alt $ do
                            guard $ isPrefixOf l longRest
                            r <- case List.drop len longRest of
                              ""      -> return ""
                              (' ':r) -> return r
                              _ -> mzero
                            return $ (l, r, f) of
            Nothing -> mTell ["could not understand flag at --" ++ longRest]
            Just (flagStr, flagRest, flag) ->
              if length (_flag_params flag) /= 0
                then error "flag params not supported yet!"
                else do
                  mSet flagRest
                  mModify $ Map.insertWith (++)
                                  (_flag_unique flag)
                                  [FlagParsedElement [flagStr]]
        ('-':shortRest) ->
          case shortRest of
            (c:' ':r) ->
              case getAlt $ mconcat $ flags <&> \f
                         -> mconcat $ _flag_short f <&> \s
                         -> Alt $ do
                              guard $ c==s
                              r' <- case r of
                                (' ':r') -> return r'
                                _ -> mzero
                              return (c, r', f) of
                Nothing -> mTell ["could not understand flag at -" ++ shortRest]
                Just (flagChr, flagRest, flag) ->
                  if length (_flag_params flag) /= 0
                    then error "flag params not supported yet!"
                    else do
                      mSet flagRest
                      mModify $ Map.insertWith (++)
                                  (_flag_unique flag)
                                  [FlagParsedElement ["-"++[flagChr]]]
            _ -> mTell ["could not parse flag at -" ++ shortRest]
        _ -> pure ()
    dropSpaces :: MonadMultiState String m => m ()
    dropSpaces = mModify $ dropWhile Char.isSpace
    processFlag :: Unique -> [Char] -> [String] -> FlagBuilder b -> Flag
    processFlag unique shorts longs flagBuilder
      = flip StateS.execState (Flag unique shorts longs Nothing [])
      $ iterM iterFuncFlag flagBuilder
      where
        iterFuncFlag :: FlagBuilderF ((StateS.State Flag) a)
                     -> (StateS.State Flag) a
        iterFuncFlag = \case
          FlagBuilderHelp h next -> flag_help .= Just h >> next
          FlagBuilderParam s p next -> do
            let param = processParam p
            flag_params %= (++ [ParamA s param])
            next $ paramStaticDef
    processParam :: Show p => ParamBuilder p () -> Param p
    processParam paramBuilder = flip StateS.execState emptyParam
                            $ runEitherT
                            $ iterM iterFuncParam paramBuilder
      where
        iterFuncParam :: Show p
                      => ParamBuilderF p (EitherT (Maybe String) (StateS.State (Param p)) ())
                      -> EitherT (Maybe String) (StateS.State (Param p)) ()
        iterFuncParam = \case
          ParamBuilderHelp h next -> do
            param <- State.Class.get
            case _param_help param of
              Nothing ->
                param_help .= Just h
              Just{} ->
                left $ Just $ "help is already defined when trying to add help \"" ++ h ++ "\""
            next
          ParamBuilderDef d next -> do
            param <- State.Class.get
            case _param_def param of
              Nothing ->
                param_def .= Just d
              Just{} ->
                left $ Just $ "default is already defined when trying to add default \"" ++ show d ++ "\""
            next

cmdActionPartial :: Command out -> Either String out
cmdActionPartial = maybe (Left err) Right . _cmd_run
  where
    err = "command is missing implementation!"

cmdAction :: String -> CmdBuilder out () -> Either String out
cmdAction s b = case cmdGetPartial s b of
  ([], _, cmd)    -> cmdActionPartial cmd
  ((out:_), _, _) -> Left $ out
  
ppCommand :: Command out -> String
ppCommand cmd
    = PP.render
    $ PP.vcat
      [ case _cmd_help cmd of
          Nothing -> PP.empty
          Just x -> PP.text x
      , case _cmd_children cmd of
          [] -> PP.empty
          cs -> PP.text "commands:" PP.$$ PP.nest 2 (PP.vcat $ commandShort <$> cs)
      , case _cmd_flags cmd of
          [] -> PP.empty
          fs -> PP.text "flags:" PP.$$ PP.nest 2 (PP.vcat $ flagShort <$> fs)
      ]
  where
    commandShort :: (String, Command out) -> PP.Doc
    commandShort (s, c)
      =     PP.text (s ++ ((_cmd_params c) >>= \(ParamA ps _) -> " " ++ ps))
      PP.<> case _cmd_help c of
              Nothing -> PP.empty
              Just h  -> PP.text ":" PP.<+> PP.text h
    flagShort :: Flag -> PP.Doc
    flagShort f = PP.hsep (PP.text . ("-"++) . return <$> _flag_short f)
           PP.<+> PP.hsep (PP.text . ("--"++)         <$> _flag_long f)
           PP.<+> case _flag_help f of
                    Nothing -> PP.empty
                    Just h -> PP.text h

ppCommandShort :: Command out -> String
ppCommandShort cmd
    = PP.render
    $ printParent cmd
      PP.<+>
      case _cmd_flags cmd of
        [] -> PP.empty
        fs -> tooLongText 20 "[FLAGS]" $ List.unwords $ fs <&> \f ->
                   "["
                ++ (List.unwords $ (_flag_short f <&> \c -> ['-', c])
                                ++ (_flag_long  f <&> \l -> "--" ++ l)
                   )
                ++ "]"
      PP.<+>
      case _cmd_params cmd of
        [] -> PP.empty
        ps -> PP.text $ List.unwords $ ps <&> \(ParamA s _) -> Char.toUpper <$> s
      PP.<+>
      case _cmd_children cmd of
        [] -> PP.empty
        cs -> PP.text
            $ if Maybe.isJust $ _cmd_run cmd
                then "[<" ++ intercalate "|" (fst <$> cs) ++ ">]"
                else "<" ++ intercalate "|" (fst <$> cs) ++ ">"
 where
  printParent :: Command out -> PP.Doc
  printParent c = case _cmd_mParent c of
    Nothing -> PP.empty
    Just (p, x) -> printParent p PP.<+> PP.text x

ppCommandShortHelp :: Command out -> String
ppCommandShortHelp cmd
    = PP.render
    $ printParent cmd
      PP.<+>
      case _cmd_flags cmd of
        [] -> PP.empty
        fs -> tooLongText 20 "[FLAGS]" $ List.unwords $ fs <&> \f ->
                   "["
                ++ (List.unwords $ (_flag_short f <&> \c -> ['-', c])
                                ++ (_flag_long  f <&> \l -> "--" ++ l)
                   )
                ++ "]"
      PP.<+>
      case _cmd_params cmd of
        [] -> PP.empty
        ps -> PP.text $ List.unwords $ ps <&> \(ParamA s _) -> Char.toUpper <$> s
      PP.<+>
      case _cmd_children cmd of
        [] -> PP.empty
        cs -> PP.text
            $ if Maybe.isJust $ _cmd_run cmd
                then "[<" ++ intercalate "|" (fst <$> cs) ++ ">]"
                else "<" ++ intercalate "|" (fst <$> cs) ++ ">"
      PP.<>
      case _cmd_help cmd of
        Nothing -> PP.empty
        Just h  -> PP.text ":" PP.<+> PP.text h
 where
  printParent :: Command out -> PP.Doc
  printParent c = case _cmd_mParent c of
    Nothing -> PP.empty
    Just (p, x) -> printParent p PP.<+> PP.text x

tooLongText :: Int -- max length
            -> String -- alternative if actual length is bigger than max.
            -> String -- text to print, if length is fine.
            -> PP.Doc
tooLongText i alt s = PP.text $ Bool.bool alt s $ null $ drop i s
 
cmds :: CmdBuilder (IO ()) ()
cmds = do
  addCmd "echo" $ do
    help "print its parameter to output"
    str <- addParam "string" $ do
      help "the string to print"
      -- def "foo"
    impl $ do
      putStrLn str
  addCmd "hello" $ do
    help "prints some greeting"
    short <- flagAsBool $ addFlag "" ["short"] $ return ()
    name <- addParam "name" $ do
      help "your name, so you can be greeted properly"
      def "user"
    impl $ do
      if short
        then putStrLn $ "hi, " ++ name ++"!"
        else putStrLn $ "hello, " ++ name ++", welcome to colint!"

flagAsBool :: CmdBuilder m [a] -> CmdBuilder m Bool
flagAsBool = fmap (not . null)

main :: IO ()
main = do
  case cmdCheckNonStatic cmds of
    Just err -> do
      putStrLn "error building commands!!"
      putStrLn err
    Nothing -> do
      forever $ do
        putStr "> "
        hFlush stdout
        input <- System.IO.getLine
        let (errs, _, partial) = cmdGetPartial input cmds
        print partial
        putStrLn $ ppCommand $ partial
        case (errs, cmdActionPartial partial) of
          (err:_, _) -> print err
          ([], eEff) -> case eEff of
            Left err -> do
              putStrLn $ "could not interpret input: " ++ err
            Right eff -> do
              eff

-- ----

instance IsParam String where
  paramParse s = do
    let s1 = dropWhile Char.isSpace s
    let (param, rest) = List.span (not . Char.isSpace) s1
    guard $ not $ null param
    pure $ (param, param, rest) -- we remove trailing whitespace, evil as we are.
  paramStaticDef = ""

instance IsParam () where
  paramParse s = do
    let s1 = dropWhile Char.isSpace s
    rest <- List.stripPrefix "()" s1
    pure $ ((), "()", rest)
  paramStaticDef = ()
