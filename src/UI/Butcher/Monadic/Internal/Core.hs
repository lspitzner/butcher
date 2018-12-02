{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module UI.Butcher.Monadic.Internal.Core
  ( addCmdSynopsis
  , addCmdHelp
  , addCmdHelpStr
  , peekCmdDesc
  , peekInput
  , addCmdPart
  , addCmdPartA
  , addCmdPartMany
  , addCmdPartManyA
  , addCmdPartInp
  , addCmdPartInpA
  , addCmdPartManyInp
  , addCmdPartManyInpA
  , addCmd
  , addCmdHidden
  , addNullCmd
  , addCmdImpl
  , addAlternatives
  , reorderStart
  , reorderStop
  , checkCmdParser
  , runCmdParser
  , runCmdParserExt
  , runCmdParserA
  , runCmdParserAExt
  , mapOut
  , varPartDesc
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict
                                               as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict
                                               as MultiStateS

import qualified Lens.Micro                    as Lens
import           Lens.Micro                     ( (%~)
                                                , (.~)
                                                )

import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint               ( (<+>)
                                                , ($$)
                                                , ($+$)
                                                )

import           Data.HList.ContainsType

import           Data.Dynamic

import           UI.Butcher.Monadic.Internal.Types



-- general-purpose helpers
----------------------------

mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = mGet >>= mSet . f

-- sadly, you need a degree in type inference to know when we can use
-- these operators and when it must be avoided due to type ambiguities
-- arising around s in the signatures below. That's the price of not having
-- the functional dependency in MonadMulti*T.

(.=+) :: MonadMultiState s m => Lens.ASetter s s a b -> b -> m ()
l .=+ b = mModify $ l .~ b

(%=+) :: MonadMultiState s m => Lens.ASetter s s a b -> (a -> b) -> m ()
l %=+ f = mModify (l %~ f)

-- inflateStateProxy :: (Monad m, ContainsType s ss)
--                   => p s -> StateS.StateT s m a -> MultiRWSS.MultiRWST r w ss m a
-- inflateStateProxy _ = MultiRWSS.inflateState

-- more on-topic stuff
----------------------------

-- instance IsHelpBuilder (CmdBuilder out) where
--   help s = liftF $ CmdBuilderHelp s ()
-- 
-- instance IsHelpBuilder (ParamBuilder p) where
--   help s = liftF $ ParamBuilderHelp s ()
-- 
-- instance IsHelpBuilder FlagBuilder where
--   help s = liftF $ FlagBuilderHelp s ()

-- | Add a synopsis to the command currently in scope; at top level this will
-- be the implicit top-level command.
--
-- Adding a second synopsis will overwrite a previous synopsis;
-- 'checkCmdParser' will check that you don't (accidentally) do this however.
addCmdSynopsis :: String -> CmdParser f out ()
addCmdSynopsis s = liftF $ CmdParserSynopsis s ()

-- | Add a help document to the command currently in scope; at top level this
-- will be the implicit top-level command.
--
-- Adding a second document will overwrite a previous document;
-- 'checkCmdParser' will check that you don't (accidentally) do this however.
addCmdHelp :: PP.Doc -> CmdParser f out ()
addCmdHelp s = liftF $ CmdParserHelp s ()

-- | Like @'addCmdHelp' . PP.text@
addCmdHelpStr :: String -> CmdParser f out ()
addCmdHelpStr s = liftF $ CmdParserHelp (PP.text s) ()

-- | Semi-hacky way of accessing the output CommandDesc from inside of a
-- 'CmdParser'. This is not implemented via knot-tying, i.e. the CommandDesc
-- you get is _not_ equivalent to the CommandDesc returned by 'runCmdParser'.
-- Also see 'runCmdParserWithHelpDesc' which does knot-tying.
--
-- For best results, use this "below"
-- any 'addCmd' invocations in the current context, e.g. directly before
-- the 'addCmdImpl' invocation.
peekCmdDesc :: CmdParser f out (CommandDesc ())
peekCmdDesc = liftF $ CmdParserPeekDesc id

-- | Semi-hacky way of accessing the current input that is not yet processed.
-- This must not be used to do any parsing. The purpose of this function is
-- to provide a String to be used for output to the user, as feedback about
-- what command was executed. For example we may think of an interactive
-- program reacting to commandline input such as
-- "run --delay 60 fire-rockets" which shows a 60 second delay on the
-- "fire-rockets" command. The latter string could have been obtained
-- via 'peekInput' after having parsed "run --delay 60" already.
peekInput :: CmdParser f out String
peekInput = liftF $ CmdParserPeekInput id

-- | Add part that is expected to occur exactly once in the input. May
-- succeed on empty input (e.g. by having a default).
addCmdPart
  :: (Applicative f, Typeable p)
  => PartDesc
  -> (String -> Maybe (p, String))
  -> CmdParser f out p
addCmdPart p f = liftF $ CmdParserPart p f (\_ -> pure ()) id

addCmdPartA
  :: (Typeable p)
  => PartDesc
  -> (String -> Maybe (p, String))
  -> (p -> f ())
  -> CmdParser f out p
addCmdPartA p f a = liftF $ CmdParserPart p f a id

-- | Add part that is not required to occur, and can occur as often as
-- indicated by 'ManyUpperBound'. Must not succeed on empty input.
addCmdPartMany
  :: (Applicative f, Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> (String -> Maybe (p, String))
  -> CmdParser f out [p]
addCmdPartMany b p f = liftF $ CmdParserPartMany b p f (\_ -> pure ()) id

addCmdPartManyA
  :: (Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> (String -> Maybe (p, String))
  -> (p -> f ())
  -> CmdParser f out [p]
addCmdPartManyA b p f a = liftF $ CmdParserPartMany b p f a id

-- | Add part that is expected to occur exactly once in the input. May
-- succeed on empty input (e.g. by having a default).
--
-- Only difference to 'addCmdPart' is that it accepts 'Input', i.e. can
-- behave differently for @String@ and @[String]@ input.
addCmdPartInp
  :: (Applicative f, Typeable p)
  => PartDesc
  -> (Input -> Maybe (p, Input))
  -> CmdParser f out p
addCmdPartInp p f = liftF $ CmdParserPartInp p f (\_ -> pure ()) id

addCmdPartInpA
  :: (Typeable p)
  => PartDesc
  -> (Input -> Maybe (p, Input))
  -> (p -> f ())
  -> CmdParser f out p
addCmdPartInpA p f a = liftF $ CmdParserPartInp p f a id

-- | Add part that is not required to occur, and can occur as often as
-- indicated by 'ManyUpperBound'. Must not succeed on empty input.
--
-- Only difference to 'addCmdPart' is that it accepts 'Input', i.e. can
-- behave differently for @String@ and @[String]@ input.
addCmdPartManyInp
  :: (Applicative f, Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> (Input -> Maybe (p, Input))
  -> CmdParser f out [p]
addCmdPartManyInp b p f = liftF $ CmdParserPartManyInp b p f (\_ -> pure ()) id

addCmdPartManyInpA
  :: (Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> (Input -> Maybe (p, Input))
  -> (p -> f ())
  -> CmdParser f out [p]
addCmdPartManyInpA b p f a = liftF $ CmdParserPartManyInp b p f a id

-- | Add a new child command in the current context.
addCmd
  :: Applicative f
  => String -- ^ command name
  -> CmdParser f out () -- ^ subcommand
  -> CmdParser f out ()
addCmd str sub = liftF $ CmdParserChild (Just str) Visible sub (pure ()) ()

-- | Add a new child command in the current context, but make it hidden. It
-- will not appear in docs/help generated by e.g. the functions in the
-- @Pretty@ module.
--
-- This feature is not well tested yet.
addCmdHidden
  :: Applicative f
  => String -- ^ command name
  -> CmdParser f out () -- ^ subcommand
  -> CmdParser f out ()
addCmdHidden str sub =
  liftF $ CmdParserChild (Just str) Hidden sub (pure ()) ()

-- | Add a list of sub-parsers one of which will be selected and used based
-- on the provided predicate function. The input elements consist of:
-- a) a name used for the command description of the output,
-- b) a predicate function; the first True predicate determines which element
--    to apply
-- c) a CmdParser.
addAlternatives
  :: Typeable p
  => [(String, String -> Bool, CmdParser f out p)]
  -> CmdParser f out p
addAlternatives elems = liftF $ CmdParserAlternatives desc alts id
 where
  desc = PartAlts $ [PartVariable s | (s, _, _) <- elems]
  alts = [(a, b) | (_, a, b) <- elems]

-- | Create a simple PartDesc from a string.
varPartDesc :: String -> PartDesc
varPartDesc = PartVariable

-- | Add a new nameless child command in the current context. Nameless means
-- that this command matches the empty input, i.e. will always apply.
-- This feature is experimental and CommandDesc pretty-printing might not
-- correctly in presense of nullCmds.
addNullCmd :: Applicative f => CmdParser f out () -> CmdParser f out ()
addNullCmd sub = liftF $ CmdParserChild Nothing Hidden sub (pure ()) ()

-- | Add an implementation to the current command.
addCmdImpl :: out -> CmdParser f out ()
addCmdImpl o = liftF $ CmdParserImpl o ()

-- | Best explained via example:
--
-- > do
-- >   reorderStart
-- >   bright <- addSimpleBoolFlag "" ["bright"] mempty
-- >   yellow <- addSimpleBoolFlag "" ["yellow"] mempty
-- >   reorderStop
-- >   ..
--
-- will accept any inputs "" "--bright" "--yellow" "--bright --yellow" "--yellow --bright".
--
-- This works for any flags/params, but bear in mind that the results might
-- be unexpected because params may match on any input.
--
-- Note that start/stop must occur in pairs, and it will be a runtime error
-- if you mess this up. Use 'checkCmdParser' if you want to check all parts
-- of your 'CmdParser' without providing inputs that provide 100% coverage.
reorderStart :: CmdParser f out ()
reorderStart = liftF $ CmdParserReorderStart ()

-- | See 'reorderStart'
reorderStop :: CmdParser f out ()
reorderStop = liftF $ CmdParserReorderStop ()

-- addPartHelp :: String -> CmdPartParser ()
-- addPartHelp s = liftF $ CmdPartParserHelp s ()
-- 
-- addPartParserBasic :: (String -> Maybe (p, String)) -> Maybe p -> CmdPartParser p
-- addPartParserBasic f def = liftF $ CmdPartParserCore f def id
-- 
-- addPartParserOptionalBasic :: CmdPartParser p -> CmdPartParser (Maybe p)
-- addPartParserOptionalBasic p = liftF $ CmdPartParserOptional p id

data PartGatherData f
  = forall p . Typeable p => PartGatherData
    { _pgd_id     :: Int
    , _pgd_desc   :: PartDesc
    , _pgd_parseF :: Either (String -> Maybe (p, String))
                            (Input  -> Maybe (p, Input))
    , _pgd_act    :: p -> f ()
    , _pgd_many   :: Bool
    }

data ChildGather f out =
  ChildGather (Maybe String) Visibility (CmdParser f out ()) (f ())

type PartParsedData = Map Int [Dynamic]

data CmdDescStack = StackBottom (Deque PartDesc)
                  | StackLayer  (Deque PartDesc) String CmdDescStack

descStackAdd :: PartDesc -> CmdDescStack -> CmdDescStack
descStackAdd d = \case
  StackBottom l    -> StackBottom $ Deque.snoc d l
  StackLayer l s u -> StackLayer (Deque.snoc d l) s u


-- | Because butcher is evil (i.e. has constraints not encoded in the types;
-- see the README), this method can be used as a rough check that you did not
-- mess up. It traverses all possible parts of the 'CmdParser' thereby
-- ensuring that the 'CmdParser' has a valid structure.
--
-- This method also yields a _complete_ @CommandDesc@ output, where the other
-- runCmdParser* functions all traverse only a shallow structure around the
-- parts of the 'CmdParser' touched while parsing the current input.
checkCmdParser
  :: forall f out
   . Maybe String -- ^ top-level command name
  -> CmdParser f out () -- ^ parser to check
  -> Either String (CommandDesc ())
checkCmdParser mTopLevel cmdParser =
  (>>= final)
    $ MultiRWSS.runMultiRWSTNil
    $ MultiRWSS.withMultiStateAS (StackBottom mempty)
    $ MultiRWSS.withMultiStateS emptyCommandDesc
    $ processMain cmdParser
 where
  final :: (CommandDesc out, CmdDescStack) -> Either String (CommandDesc ())
  final (desc, stack) = case stack of
    StackBottom descs ->
      Right
        $  descFixParentsWithTopM
             (mTopLevel <&> \n -> (Just n, emptyCommandDesc))
        $  ()
        <$ desc { _cmd_parts = Data.Foldable.toList descs }
    StackLayer _ _ _ -> Left "unclosed ReorderStart or GroupStart"
  processMain
    :: CmdParser f out a
    -> MultiRWSS.MultiRWST
         '[]
         '[]
         '[CommandDesc out, CmdDescStack]
         (Either String)
         a
  processMain = \case
    Pure x                      -> return x
    Free (CmdParserHelp h next) -> do
      cmd :: CommandDesc out <- mGet
      mSet $ cmd { _cmd_help = Just h }
      processMain next
    Free (CmdParserSynopsis s next) -> do
      cmd :: CommandDesc out <- mGet
      mSet
        $ cmd { _cmd_synopsis = Just $ PP.fsep $ fmap PP.text $ List.words s }
      processMain next
    Free (CmdParserPeekDesc nextF) -> do
      processMain $ nextF monadMisuseError
    Free (CmdParserPeekInput nextF) -> do
      processMain $ nextF monadMisuseError
    Free (CmdParserPart desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd desc descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserPartInp desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd desc descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserPartMany bound desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserPartManyInp bound desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserChild cmdStr vis sub _act next) -> do
      mInitialDesc           <- takeCommandChild cmdStr
      cmd :: CommandDesc out <- mGet
      subCmd                 <- do
        stackCur :: CmdDescStack <- mGet
        mSet $ fromMaybe (emptyCommandDesc :: CommandDesc out) mInitialDesc
        mSet $ StackBottom mempty
        processMain sub
        c          <- mGet
        stackBelow <- mGet
        mSet cmd
        mSet stackCur
        subParts <- case stackBelow of
          StackBottom descs -> return $ Data.Foldable.toList descs
          StackLayer _ _ _  -> lift $ Left "unclosed ReorderStart or GroupStart"
        return c { _cmd_parts = subParts, _cmd_visibility = vis }
      mSet $ cmd
        { _cmd_children = (cmdStr, subCmd) `Deque.snoc` _cmd_children cmd
        }
      processMain next
    Free (CmdParserImpl out next) -> do
      cmd_out .=+ Just out
      processMain $ next
    Free (CmdParserGrouped groupName next) -> do
      stackCur <- mGet
      mSet $ StackLayer mempty groupName stackCur
      processMain $ next
    Free (CmdParserGroupEnd next) -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> do
          lift $ Left $ "butcher interface error: group end without group start"
        StackLayer _descs "" _up -> do
          lift $ Left $ "GroupEnd found, but expected ReorderStop first"
        StackLayer descs groupName up -> do
          mSet $ descStackAdd
            (PartRedirect groupName (PartSeq (Data.Foldable.toList descs)))
            up
          processMain $ next
    Free (CmdParserReorderStop next) -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> lift $ Left $ "ReorderStop without reorderStart"
        StackLayer descs "" up -> do
          mSet $ descStackAdd (PartReorder (Data.Foldable.toList descs)) up
        StackLayer{} ->
          lift $ Left $ "Found ReorderStop, but need GroupEnd first"
      processMain next
    Free (CmdParserReorderStart next) -> do
      stackCur <- mGet
      mSet $ StackLayer mempty "" stackCur
      processMain next
    Free (CmdParserAlternatives desc alts nextF) -> do
      mModify (descStackAdd desc)
      states <- MultiRWSS.mGetRawS
      let go
            :: [(String -> Bool, CmdParser f out p)]
            -> MultiRWSS.MultiRWST
                 '[] '[] '[CommandDesc out, CmdDescStack] (Either String) p
          go [] = lift $ Left $ "Empty alternatives"
          go [(_, alt)] = processMain alt
          go ((_, alt1):altr) = do
            case MultiRWSS.runMultiRWSTNil $ MultiRWSS.withMultiStates states (processMain alt1) of
              Left{} -> go altr
              Right (p, states') -> MultiRWSS.mPutRawS states' $> p
      p <- go alts
      processMain $ nextF p

  monadMisuseError :: a
  monadMisuseError =
    error
      $  "CmdParser definition error -"
      ++ " used Monad powers where only Applicative/Arrow is allowed"

newtype PastCommandInput = PastCommandInput Input


-- | Run a @CmdParser@ on the given input, returning:
--
-- a) A @CommandDesc ()@ that accurately represents the subcommand that was
--    reached, even if parsing failed. Because this is returned always, the
--    argument is @()@ because "out" requires a successful parse.
--
-- b) Either an error or the result of a successful parse, including a proper
--    "CommandDesc out" from which an "out" can be extracted (presuming that
--    the command has an implementation).
runCmdParser
  :: Maybe String -- ^ program name to be used for the top-level @CommandDesc@
  -> Input -- ^ input to be processed
  -> CmdParser Identity out () -- ^ parser to use
  -> (CommandDesc (), Either ParsingError (CommandDesc out))
runCmdParser mTopLevel inputInitial cmdParser =
  runIdentity $ runCmdParserA mTopLevel inputInitial cmdParser

-- | Like 'runCmdParser', but also returning all input after the last
-- successfully parsed subcommand. E.g. for some input
-- "myprog foo bar -v --wrong" where parsing fails at "--wrong", this will
-- contain the full "-v --wrong". Useful for interactive feedback stuff.
runCmdParserExt
  :: Maybe String -- ^ program name to be used for the top-level @CommandDesc@
  -> Input -- ^ input to be processed
  -> CmdParser Identity out () -- ^ parser to use
  -> (CommandDesc (), Input, Either ParsingError (CommandDesc out))
runCmdParserExt mTopLevel inputInitial cmdParser =
  runIdentity $ runCmdParserAExt mTopLevel inputInitial cmdParser

-- | The Applicative-enabled version of 'runCmdParser'.
runCmdParserA
  :: forall f out
   . Applicative f
  => Maybe String -- ^ program name to be used for the top-level @CommandDesc@
  -> Input -- ^ input to be processed
  -> CmdParser f out () -- ^ parser to use
  -> f (CommandDesc (), Either ParsingError (CommandDesc out))
runCmdParserA mTopLevel inputInitial cmdParser =
  (\(x, _, z) -> (x, z)) <$> runCmdParserAExt mTopLevel inputInitial cmdParser

-- | The Applicative-enabled version of 'runCmdParserExt'.
runCmdParserAExt
  :: forall f out
   . Applicative f
  => Maybe String -- ^ program name to be used for the top-level @CommandDesc@
  -> Input -- ^ input to be processed
  -> CmdParser f out () -- ^ parser to use
  -> f
       ( CommandDesc ()
       , Input
       , Either ParsingError (CommandDesc out)
       )
runCmdParserAExt mTopLevel inputInitial cmdParser =
  runIdentity
    $ MultiRWSS.runMultiRWSTNil
    $ (<&> captureFinal)
    $ MultiRWSS.withMultiWriterWA
    $ MultiRWSS.withMultiStateA cmdParser
    $ MultiRWSS.withMultiStateSA (StackBottom mempty)
    $ MultiRWSS.withMultiStateSA inputInitial
    $ MultiRWSS.withMultiStateSA (PastCommandInput inputInitial)
    $ MultiRWSS.withMultiStateSA initialCommandDesc
    $ processMain cmdParser
 where
  initialCommandDesc = emptyCommandDesc
    { _cmd_mParent = mTopLevel <&> \n -> (Just n, emptyCommandDesc)
    }
  captureFinal
    :: ( [String]
       , (CmdDescStack, (Input, (PastCommandInput, (CommandDesc out, f ()))))
       )
    -> f (CommandDesc (), Input, Either ParsingError (CommandDesc out))
  captureFinal tuple1 = act $> (() <$ cmd', pastCmdInput, res)
   where
    (errs                         , tuple2) = tuple1
    (descStack                    , tuple3) = tuple2
    (inputRest                    , tuple4) = tuple3
    (PastCommandInput pastCmdInput, tuple5) = tuple4
    (cmd                          , act   ) = tuple5
    errs'     = errs ++ inputErrs ++ stackErrs
    inputErrs = case inputRest of
      InputString s | all Char.isSpace s -> []
      InputString{} -> ["could not parse input/unprocessed input"]
      InputArgs [] -> []
      InputArgs{} -> ["could not parse input/unprocessed input"]
    stackErrs = case descStack of
      StackBottom{} -> []
      _             -> ["butcher interface error: unclosed group"]
    cmd' = postProcessCmd descStack cmd
    res =
      if null errs' then Right cmd' else Left $ ParsingError errs' inputRest
  processMain
    :: -- forall a
       CmdParser f out ()
    -> MultiRWSS.MultiRWS
         '[]
         '[[String]]
         '[CommandDesc out, PastCommandInput, Input, CmdDescStack, CmdParser
           f
           out
           ()]
         (f ())
  processMain = \case
    Pure ()                     -> return $ pure ()
    Free (CmdParserHelp h next) -> do
      cmd :: CommandDesc out <- mGet
      mSet $ cmd { _cmd_help = Just h }
      processMain next
    Free (CmdParserSynopsis s next) -> do
      cmd :: CommandDesc out <- mGet
      mSet
        $ cmd { _cmd_synopsis = Just $ PP.fsep $ fmap PP.text $ List.words s }
      processMain next
    Free (CmdParserPeekDesc nextF) -> do
      parser :: CmdParser f out () <- mGet
      -- partialDesc :: CommandDesc out <- mGet
      -- partialStack :: CmdDescStack <- mGet
      -- run the rest without affecting the actual stack
      -- to retrieve the complete cmddesc.
      cmdCur :: CommandDesc out <- mGet
      let (cmd :: CommandDesc out, stack) =
            runIdentity
              $ MultiRWSS.runMultiRWSTNil
              $ MultiRWSS.withMultiStateSA emptyCommandDesc
                  { _cmd_mParent = _cmd_mParent cmdCur
                  } -- partialDesc
              $ MultiRWSS.withMultiStateS (StackBottom mempty) -- partialStack
              $ iterM processCmdShallow
              $ parser
      processMain $ nextF $ () <$ postProcessCmd stack cmd
    Free (CmdParserPeekInput nextF) -> do
      processMain $ nextF $ inputToString inputInitial
    Free (CmdParserPart desc parseF actF nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd desc descStack
      input <- mGet
      case input of
        InputString str -> case parseF str of
          Just (x, rest) -> do
            mSet $ InputString rest
            actRest <- processMain $ nextF x
            return $ actF x *> actRest
          Nothing -> do
            mTell ["could not parse " ++ getPartSeqDescPositionName desc]
            processMain $ nextF monadMisuseError
        InputArgs (str:strr) -> case parseF str of
          Just (x, "") -> do
            mSet $ InputArgs strr
            actRest <- processMain $ nextF x
            return $ actF x *> actRest
          _ -> do
            mTell ["could not parse " ++ getPartSeqDescPositionName desc]
            processMain $ nextF monadMisuseError
        InputArgs [] -> do
          mTell ["could not parse " ++ getPartSeqDescPositionName desc]
          processMain $ nextF monadMisuseError
    Free (CmdParserPartInp desc parseF actF nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd desc descStack
      input <- mGet
      case parseF input of
        Just (x, rest) -> do
          mSet $ rest
          actRest <- processMain $ nextF x
          return $ actF x *> actRest
        Nothing -> do
          mTell ["could not parse " ++ getPartSeqDescPositionName desc]
          processMain $ nextF monadMisuseError
    Free (CmdParserPartMany bound desc parseF actF nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) descStack
      let proc = do
            dropSpaces
            input <- mGet
            case input of
              InputString str -> case parseF str of
                Just (x, r) -> do
                  mSet $ InputString r
                  xr <- proc
                  return $ x : xr
                Nothing -> return []
              InputArgs (str:strr) -> case parseF str of
                Just (x, "") -> do
                  mSet $ InputArgs strr
                  xr <- proc
                  return $ x : xr
                _ -> return []
              InputArgs [] -> return []
      r <- proc
      let act = traverse actF r
      (act *>) <$> processMain (nextF $ r)
    Free (CmdParserPartManyInp bound desc parseF actF nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) descStack
      let proc = do
            dropSpaces
            input <- mGet
            case parseF input of
              Just (x, r) -> do
                mSet $ r
                xr <- proc
                return $ x : xr
              Nothing -> return []
      r <- proc
      let act = traverse actF r
      (act *>) <$> processMain (nextF $ r)
    f@(Free (CmdParserChild _ _ _ _ _)) -> do
      dropSpaces
      input <- mGet
      (gatheredChildren :: [ChildGather f out], restCmdParser) <-
        MultiRWSS.withMultiWriterWA $ childrenGather f
      let
        child_fold
          :: ( Deque (Maybe String)
             , Map (Maybe String) (Visibility, CmdParser f out (), f ())
             )
          -> ChildGather f out
          -> ( Deque (Maybe String)
             , Map (Maybe String) (Visibility, CmdParser f out (), f ())
             )
        child_fold (c_names, c_map) (ChildGather name vis child act) =
          case name `MapS.lookup` c_map of
            Nothing ->
              ( Deque.snoc name c_names
              , MapS.insert name (vis, child, act) c_map
              )
            Just (vis', child', act') ->
              ( c_names
              , MapS.insert name (vis', child' >> child, act') c_map
                 -- we intentionally override/ignore act here.
                 -- TODO: it should be documented that we expect the same act
                 -- on different child nodes with the same name.
              )
        (child_name_list, child_map) =
          foldl' child_fold (mempty, MapS.empty) gatheredChildren
        combined_child_list =
          Data.Foldable.toList child_name_list <&> \n -> (n, child_map MapS.! n)
      let
        mRest = asum $ combined_child_list <&> \(mname, (child, act, vis)) ->
          case (mname, input) of
            (Just name, InputString str) | name == str ->
              Just $ (Just name, child, act, vis, InputString "")
            (Just name, InputString str) | (name ++ " ") `isPrefixOf` str ->
              Just
                $ ( Just name
                  , child
                  , act
                  , vis
                  , InputString $ drop (length name + 1) str
                  )
            (Just name, InputArgs (str:strr)) | name == str ->
              Just $ (Just name, child, act, vis, InputArgs strr)
            (Nothing, _) -> Just $ (Nothing, child, act, vis, input)
            _            -> Nothing
      combined_child_list `forM_` \(child_name, (vis, child, _)) -> do
        let initialDesc :: CommandDesc out = emptyCommandDesc
        -- get the shallow desc for the child in a separate env.
        let (subCmd, subStack) =
              runIdentity
                $ MultiRWSS.runMultiRWSTNil
                $ MultiRWSS.withMultiStateSA initialDesc
                $ MultiRWSS.withMultiStateS (StackBottom mempty)
                $ iterM processCmdShallow child
        cmd_children %=+ Deque.snoc
          ( child_name
          , postProcessCmd subStack subCmd { _cmd_visibility = vis }
          )
      case mRest of
        Nothing -> do -- a child not matching what we have in the input
          -- get the shallow desc for the child in a separate env.
          -- proceed regularly on the same layer
          processMain $ restCmdParser
        Just (name, vis, child, act, rest) -> do -- matching child -> descend
          -- process all remaining stuff on the same layer shallowly,
          -- including the current node. This will walk over the child
          -- definition(s) again, but that is harmless because we do not
          -- overwrite them.
          iterM processCmdShallow f
          -- do the descend
          cmd <- do
            c :: CommandDesc out      <- mGet
            prevStack :: CmdDescStack <- mGet
            return $ postProcessCmd prevStack c
          mSet $ rest
          mSet $ PastCommandInput rest
          mSet $ emptyCommandDesc { _cmd_mParent    = Just (name, cmd)
                                  , _cmd_visibility = vis
                                  }
          mSet $ child
          mSet $ StackBottom mempty
          childAct <- processMain child
          -- check that descending yielded
          return $ act *> childAct
    Free (CmdParserImpl out next) -> do
      cmd_out .=+ Just out
      processMain $ next
    Free (CmdParserGrouped groupName next) -> do
      stackCur <- mGet
      mSet $ StackLayer mempty groupName stackCur
      processMain $ next
    Free (CmdParserGroupEnd next) -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> do
          mTell $ ["butcher interface error: group end without group start"]
          return $ pure () -- hard abort should be fine for this case.
        StackLayer descs groupName up -> do
          mSet $ descStackAdd
            (PartRedirect groupName (PartSeq (Data.Foldable.toList descs)))
            up
          processMain $ next
    Free (CmdParserReorderStop next) -> do
      mTell $ ["butcher interface error: reorder stop without reorder start"]
      processMain next
    Free (CmdParserReorderStart next) -> do
      reorderData <-
        MultiRWSS.withMultiStateA (1 :: Int)
        $ MultiRWSS.withMultiWriterW
        $ iterM reorderPartGather
        $ next
      let
        reorderMapInit :: Map Int (PartGatherData f)
        reorderMapInit = MapS.fromList $ reorderData <&> \d -> (_pgd_id d, d)
        tryParsePartData
          :: Input
          -> PartGatherData f
          -> First (Int, Dynamic, Input, Bool, f ())
        tryParsePartData input (PartGatherData pid _ pfe act allowMany) = First
          [ (pid, toDyn r, rest, allowMany, act r)
          | (r, rest) <- case pfe of
            Left pfStr -> case input of
              InputString str -> case pfStr str of
                Just (x, r) | r /= str -> Just (x, InputString r)
                _                      -> Nothing
              InputArgs (str:strr) -> case pfStr str of
                Just (x, "") -> Just (x, InputArgs strr)
                _            -> Nothing
              InputArgs [] -> Nothing
            Right pfInp -> case pfInp input of
              Just (x, r) | r /= input -> Just (x, r)
              _                        -> Nothing
          ]
        parseLoop = do
          input                           <- mGet
          m :: Map Int (PartGatherData f) <- mGet
          case getFirst $ Data.Foldable.foldMap (tryParsePartData input) m of
                     -- i will be angry if foldMap ever decides to not fold
                     -- in order of keys.
            Nothing                        -> return $ pure ()
            Just (pid, x, rest, more, act) -> do
              mSet rest
              mModify $ MapS.insertWith (++) pid [x]
              when (not more) $ do
                mSet $ MapS.delete pid m
              actRest <- parseLoop
              return $ act *> actRest
      (finalMap, (fr, acts)) <-
        MultiRWSS.withMultiStateSA (MapS.empty :: PartParsedData)
        $ MultiRWSS.withMultiStateA reorderMapInit
        $ do
            acts     <- parseLoop -- filling the map
            stackCur <- mGet
            mSet $ StackLayer mempty "" stackCur
            fr <- MultiRWSS.withMultiStateA (1 :: Int) $ processParsedParts next
            return (fr, acts)
      -- we check that all data placed in the map has been consumed while
      -- running the parts for which we collected the parseresults.
      -- there can only be any rest if the collection of parts changed
      -- between the reorderPartGather traversal and the processParsedParts
      -- consumption.
      if MapS.null finalMap
        then do
          actRest <- processMain fr
          return $ acts *> actRest
        else monadMisuseError
    Free (CmdParserAlternatives desc alts nextF) -> do
      input :: Input <- mGet
      case input of
        InputString str
          | Just (_, sub) <- find (\(predicate, _sub) -> predicate str) alts ->
              processMain $ sub >>= nextF
        InputArgs (str:_)
          | Just (_, sub) <- find (\(predicate, _sub) -> predicate str) alts ->
              processMain $ sub >>= nextF
        _ -> do
          mTell ["could not parse any of " ++ getPartSeqDescPositionName desc]
          processMain $ nextF monadMisuseError

  reorderPartGather
    :: ( MonadMultiState Int m
       , MonadMultiWriter [PartGatherData f] m
       , MonadMultiWriter [String] m
       )
    => CmdParserF f out (m ())
    -> m ()
  reorderPartGather = \case
    -- TODO: why do PartGatherData contain desc?
    CmdParserPart desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Left parseF) actF False]
      nextF $ monadMisuseError
    CmdParserPartInp desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Right parseF) actF False]
      nextF $ monadMisuseError
    CmdParserPartMany _ desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Left parseF) actF True]
      nextF $ monadMisuseError
    CmdParserPartManyInp _ desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Right parseF) actF True]
      nextF $ monadMisuseError
    CmdParserReorderStop _next -> do
      return ()
    CmdParserHelp{}         -> restCase
    CmdParserSynopsis{}     -> restCase
    CmdParserPeekDesc{}     -> restCase
    CmdParserPeekInput{}    -> restCase
    CmdParserChild{}        -> restCase
    CmdParserImpl{}         -> restCase
    CmdParserReorderStart{} -> restCase
    CmdParserGrouped{}      -> restCase
    CmdParserGroupEnd{}     -> restCase
    CmdParserAlternatives{} -> restCase
   where
    restCase = do
      mTell ["Did not find expected ReorderStop after the reordered parts"]
      return ()

  childrenGather
    :: ( MonadMultiWriter [ChildGather f out] m
       , MonadMultiState (CmdParser f out ()) m
       , MonadMultiState (CommandDesc out) m
       )
    => CmdParser f out a
    -> m (CmdParser f out a)
  childrenGather = \case
    Free (CmdParserChild cmdStr vis sub act next) -> do
      mTell [ChildGather cmdStr vis sub act]
      childrenGather next
    Free (CmdParserPeekInput nextF) -> do
      childrenGather $ nextF $ inputToString inputInitial
    Free (CmdParserPeekDesc nextF) -> do
      parser :: CmdParser f out () <- mGet
      -- partialDesc :: CommandDesc out <- mGet
      -- partialStack :: CmdDescStack <- mGet
      -- run the rest without affecting the actual stack
      -- to retrieve the complete cmddesc.
      cmdCur :: CommandDesc out <- mGet
      let (cmd :: CommandDesc out, stack) =
            runIdentity
              $ MultiRWSS.runMultiRWSTNil
              $ MultiRWSS.withMultiStateSA emptyCommandDesc
                  { _cmd_mParent = _cmd_mParent cmdCur
                  } -- partialDesc
              $ MultiRWSS.withMultiStateS (StackBottom mempty) -- partialStack
              $ iterM processCmdShallow
              $ parser
      childrenGather $ nextF $ () <$ postProcessCmd stack cmd
    something -> return something

  processParsedParts
    :: forall m r w s m0 a
     . ( MonadMultiState Int m
       , MonadMultiState PartParsedData m
       , MonadMultiState (Map Int (PartGatherData f)) m
       , MonadMultiState Input m
       , MonadMultiState (CommandDesc out) m
       , MonadMultiWriter [[Char]] m
       , m ~ MultiRWSS.MultiRWST r w s m0
       , ContainsType (CmdParser f out ()) s
       , ContainsType CmdDescStack s
       , Monad m0
       )
    => CmdParser f out a
    -> m (CmdParser f out a)
  processParsedParts = \case
    Free (CmdParserPart desc _ _ (nextF :: p -> CmdParser f out a)) ->
      part desc nextF
    Free (CmdParserPartInp desc _ _ (nextF :: p -> CmdParser f out a)) ->
      part desc nextF
    Free (CmdParserPartMany bound desc _ _ nextF) -> partMany bound desc nextF
    Free (CmdParserPartManyInp bound desc _ _ nextF) ->
      partMany bound desc nextF
    Free (CmdParserReorderStop next) -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> do
          mTell ["unexpected stackBottom"]
        StackLayer descs _ up -> do
          mSet $ descStackAdd (PartReorder (Data.Foldable.toList descs)) up
      return next
    Free (CmdParserGrouped groupName next) -> do
      stackCur <- mGet
      mSet $ StackLayer mempty groupName stackCur
      processParsedParts $ next
    Free (CmdParserGroupEnd next) -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> do
          mTell $ ["butcher interface error: group end without group start"]
          return $ next -- hard abort should be fine for this case.
        StackLayer descs groupName up -> do
          mSet $ descStackAdd
            (PartRedirect groupName (PartSeq (Data.Foldable.toList descs)))
            up
          processParsedParts $ next
    Pure x -> return $ return $ x
    f      -> do
      mTell ["Did not find expected ReorderStop after the reordered parts"]
      return f
   where
    part
      :: forall p
       . Typeable p
      => PartDesc
      -> (p -> CmdParser f out a)
      -> m (CmdParser f out a)
    part desc nextF = do
      do
        stackCur <- mGet
        mSet $ descStackAdd desc stackCur
      pid <- mGet
      mSet $ pid + 1
      parsedMap :: PartParsedData <- mGet
      mSet $ MapS.delete pid parsedMap
      partMap :: Map Int (PartGatherData f) <- mGet
      input :: Input                        <- mGet
      let
        errorResult = do
          mTell
            [ "could not parse expected input "
              ++ getPartSeqDescPositionName desc
              ++ " with remaining input: "
              ++ show input
            ]
          failureCurrentShallowRerun
          processParsedParts $ nextF monadMisuseError
        continueOrMisuse :: Maybe p -> m (CmdParser f out a)
        continueOrMisuse = maybe monadMisuseError (processParsedParts . nextF)
      case MapS.lookup pid parsedMap of
        Nothing -> case MapS.lookup pid partMap of
          Nothing                           -> monadMisuseError -- it would still be in the map
                                      -- if it never had been successfully
                                      -- parsed, as indicicated by the
                                      -- previous parsedMap Nothing lookup.
          Just (PartGatherData _ _ pfe _ _) -> case pfe of
            Left pf -> case pf "" of
              Nothing      -> errorResult
              Just (dx, _) -> continueOrMisuse $ cast dx
            Right pf -> case pf (InputArgs []) of
              Nothing      -> errorResult
              Just (dx, _) -> continueOrMisuse $ cast dx
        Just [dx] -> continueOrMisuse $ fromDynamic dx
        Just _    -> monadMisuseError
    partMany
      :: Typeable p
      => ManyUpperBound
      -> PartDesc
      -> ([p] -> CmdParser f out a)
      -> m (CmdParser f out a)
    partMany bound desc nextF = do
      do
        stackCur <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) stackCur
      pid <- mGet
      mSet $ pid + 1
      m :: PartParsedData <- mGet
      mSet $ MapS.delete pid m
      let partDyns = case MapS.lookup pid m of
            Nothing -> []
            Just r  -> reverse r
      case mapM fromDynamic partDyns of
        Nothing -> monadMisuseError
        Just xs -> processParsedParts $ nextF xs

  -- this does no error reporting at all.
  -- user needs to use check for that purpose instead.
  processCmdShallow
    :: (MonadMultiState (CommandDesc out) m, MonadMultiState CmdDescStack m)
    => CmdParserF f out (m a)
    -> m a
  processCmdShallow = \case
    CmdParserHelp h next -> do
      cmd :: CommandDesc out <- mGet
      mSet $ cmd { _cmd_help = Just h }
      next
    CmdParserSynopsis s next -> do
      cmd :: CommandDesc out <- mGet
      mSet
        $ cmd { _cmd_synopsis = Just $ PP.fsep $ fmap PP.text $ List.words s }
      next
    CmdParserPeekDesc nextF -> do
      mGet >>= nextF . fmap (\(_ :: out) -> ())
    CmdParserPeekInput nextF -> do
      nextF $ inputToString inputInitial
    CmdParserPart desc _parseF _act nextF -> do
      do
        stackCur <- mGet
        mSet $ descStackAdd desc stackCur
      nextF monadMisuseError
    CmdParserPartInp desc _parseF _act nextF -> do
      do
        stackCur <- mGet
        mSet $ descStackAdd desc stackCur
      nextF monadMisuseError
    CmdParserPartMany bound desc _parseF _act nextF -> do
      do
        stackCur <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) stackCur
      nextF monadMisuseError
    CmdParserPartManyInp bound desc _parseF _act nextF -> do
      do
        stackCur <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) stackCur
      nextF monadMisuseError
    CmdParserChild cmdStr vis _sub _act next -> do
      mExisting <- takeCommandChild cmdStr
      let childDesc :: CommandDesc out =
            fromMaybe emptyCommandDesc { _cmd_visibility = vis } mExisting
      cmd_children %=+ Deque.snoc (cmdStr, childDesc)
      next
    CmdParserImpl out next -> do
      cmd_out .=+ Just out
      next
    CmdParserGrouped groupName next -> do
      stackCur <- mGet
      mSet $ StackLayer mempty groupName stackCur
      next
    CmdParserGroupEnd next -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> pure ()
        StackLayer _descs "" _up -> pure ()
        StackLayer descs groupName up -> do
          mSet $ descStackAdd
            (PartRedirect groupName (PartSeq (Data.Foldable.toList descs)))
            up
      next
    CmdParserReorderStop next -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{}          -> return ()
        StackLayer descs "" up -> do
          mSet $ descStackAdd (PartReorder (Data.Foldable.toList descs)) up
        StackLayer{} -> return ()
      next
    CmdParserReorderStart next -> do
      stackCur <- mGet
      mSet $ StackLayer mempty "" stackCur
      next
    CmdParserAlternatives _ [] _ -> error "empty alternatives"
    CmdParserAlternatives desc ((_, alt):_) nextF -> do
      mModify (descStackAdd desc)
      nextF =<< iterM processCmdShallow alt

  failureCurrentShallowRerun
    :: ( m ~ MultiRWSS.MultiRWST r w s m0
       , MonadMultiState (CmdParser f out ()) m
       , MonadMultiState (CommandDesc out) m
       , ContainsType CmdDescStack s
       , Monad m0
       )
    => m ()
  failureCurrentShallowRerun = do
    parser :: CmdParser f out () <- mGet
    cmd :: CommandDesc out <-
      MultiRWSS.withMultiStateS emptyCommandDesc
        $ iterM processCmdShallow parser
    mSet cmd

  postProcessCmd :: CmdDescStack -> CommandDesc out -> CommandDesc out
  postProcessCmd descStack cmd = descFixParents $ cmd
    { _cmd_parts = case descStack of
      StackBottom l -> Data.Foldable.toList l
      StackLayer{}  -> []
    }

  monadMisuseError :: a
  monadMisuseError =
    error
      $  "CmdParser definition error -"
      ++ " used Monad powers where only Applicative/Arrow is allowed"


  getPartSeqDescPositionName :: PartDesc -> String
  getPartSeqDescPositionName = \case
    PartLiteral  s     -> s
    PartVariable s     -> s
    PartOptional ds'   -> f ds'
    PartAlts     alts  -> f $ head alts -- this is not optimal, but probably
                                   -- does not matter.
    PartDefault    _ d -> f d
    PartSuggestion _ d -> f d
    PartRedirect   s _ -> s
    PartMany ds        -> f ds
    PartWithHelp _ d   -> f d
    PartSeq     ds     -> List.unwords $ f <$> ds
    PartReorder ds     -> List.unwords $ f <$> ds
    PartHidden  d      -> f d
    where f = getPartSeqDescPositionName

  dropSpaces :: MonadMultiState Input m => m ()
  dropSpaces = do
    inp <- mGet
    case inp of
      InputString s -> mSet $ InputString $ dropWhile Char.isSpace s
      InputArgs{}   -> return ()

  inputToString :: Input -> String
  inputToString (InputString s ) = s
  inputToString (InputArgs   ss) = List.unwords ss

dequeLookupRemove :: Eq k => k -> Deque (k, a) -> (Maybe a, Deque (k, a))
dequeLookupRemove key deque = case Deque.uncons deque of
  Nothing             -> (Nothing, mempty)
  Just ((k, v), rest) -> if k == key
    then (Just v, rest)
    else
      let (r, rest') = dequeLookupRemove key rest
      in  (r, Deque.cons (k, v) rest')

takeCommandChild
  :: MonadMultiState (CommandDesc out) m
  => Maybe String
  -> m (Maybe (CommandDesc out))
takeCommandChild key = do
  cmd <- mGet
  let (r, children') = dequeLookupRemove key $ _cmd_children cmd
  mSet cmd { _cmd_children = children' }
  return r

-- | map over the @out@ type argument
mapOut :: (outa -> outb) -> CmdParser f outa a -> CmdParser f outb a
mapOut f = hoistFree $ \case
  CmdParserHelp     doc r     -> CmdParserHelp doc r
  CmdParserSynopsis s   r     -> CmdParserSynopsis s r
  CmdParserPeekDesc  fr       -> CmdParserPeekDesc fr
  CmdParserPeekInput fr       -> CmdParserPeekInput fr
  CmdParserPart desc fp fa fr -> CmdParserPart desc fp fa fr
  CmdParserPartMany bound desc fp fa fr ->
    CmdParserPartMany bound desc fp fa fr
  CmdParserPartInp desc fp fa fr -> CmdParserPartInp desc fp fa fr
  CmdParserPartManyInp bound desc fp fa fr ->
    CmdParserPartManyInp bound desc fp fa fr
  CmdParserChild s vis child act r ->
    CmdParserChild s vis (mapOut f child) act r
  CmdParserImpl out r               -> CmdParserImpl (f out) r
  CmdParserReorderStart r           -> CmdParserReorderStart r
  CmdParserReorderStop  r           -> CmdParserReorderStop r
  CmdParserGrouped s r              -> CmdParserGrouped s r
  CmdParserGroupEnd r               -> CmdParserGroupEnd r
  CmdParserAlternatives desc alts r -> CmdParserAlternatives
    desc
    [ (predicate, mapOut f sub) | (predicate, sub) <- alts ]
    r

-- cmdActionPartial :: CommandDesc out -> Either String out
-- cmdActionPartial = maybe (Left err) Right . _cmd_out
--   where
--     err = "command is missing implementation!"
--  
-- cmdAction :: CmdParser out () -> String -> Either String out
-- cmdAction b s = case runCmdParser Nothing s b of
--   (_, Right cmd)                     -> cmdActionPartial cmd
--   (_, Left (ParsingError (out:_) _)) -> Left $ out
--   _ -> error "whoops"
-- 
-- cmdActionRun :: (CommandDesc () -> ParsingError -> out)
--              -> CmdParser out ()
--              -> String
--              -> out
-- cmdActionRun f p s = case runCmdParser Nothing s p of
--   (cmd, Right out) -> case _cmd_out out of
--     Just o -> o
--     Nothing -> f cmd (ParsingError ["command is missing implementation!"] "")
--   (cmd, Left err) -> f cmd err

wrapBoundDesc :: ManyUpperBound -> PartDesc -> PartDesc
wrapBoundDesc ManyUpperBound1 = PartOptional
wrapBoundDesc ManyUpperBoundN = PartMany


descFixParents :: CommandDesc a -> CommandDesc a
descFixParents = descFixParentsWithTopM Nothing

-- descFixParentsWithTop :: String -> CommandDesc a -> CommandDesc a
-- descFixParentsWithTop s = descFixParentsWithTopM (Just (s, emptyCommandDesc))

descFixParentsWithTopM
  :: Maybe (Maybe String, CommandDesc a) -> CommandDesc a -> CommandDesc a
descFixParentsWithTopM mTop topDesc = Data.Function.fix $ \fixed -> topDesc
  { _cmd_mParent  = goUp fixed <$> (mTop <|> _cmd_mParent topDesc)
  , _cmd_children = _cmd_children topDesc <&> goDown fixed
  }
 where
  goUp
    :: CommandDesc a
    -> (Maybe String, CommandDesc a)
    -> (Maybe String, CommandDesc a)
  goUp child (childName, parent) =
    (,) childName $ Data.Function.fix $ \fixed -> parent
      { _cmd_mParent  = goUp fixed <$> _cmd_mParent parent
      , _cmd_children = _cmd_children parent
        <&> \(n, c) -> if n == childName then (n, child) else (n, c)
      }
  goDown
    :: CommandDesc a
    -> (Maybe String, CommandDesc a)
    -> (Maybe String, CommandDesc a)
  goDown parent (childName, child) =
    (,) childName $ Data.Function.fix $ \fixed -> child
      { _cmd_mParent  = Just (childName, parent)
      , _cmd_children = _cmd_children child <&> goDown fixed
      }


_tooLongText
  :: Int -- max length
  -> String -- alternative if actual length is bigger than max.
  -> String -- text to print, if length is fine.
  -> PP.Doc
_tooLongText i alt s = PP.text $ Bool.bool alt s $ null $ drop i s
