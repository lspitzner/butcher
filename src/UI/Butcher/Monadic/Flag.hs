
-- | Flags are arguments to your current command that are prefixed with "-" or
-- "--", for example "-v" or "--verbose". These flags can have zero or one
-- argument. (Butcher internally has more general concept of "CmdPart" that
-- could handle any number of arguments, so take this as what this module aims
-- to provide, not what you could theoretically implement on top of butcher).

-- Note that the current implementation only accepts "--foo param" but not
-- "--foo=param". Someone really ought to implement support for the latter
-- at some point :)
module UI.Butcher.Monadic.Flag
  ( Flag(..)
  , flagHelp
  , flagHelpStr
  , flagDefault
  , addSimpleBoolFlag
  , addSimpleCountFlag
  , addSimpleFlagA
  , addFlagReadParam
  , addFlagReadParams
  -- , addFlagReadParamA
  , addFlagStringParam
  , addFlagStringParams
  -- , addFlagStringParamA
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

import           Data.List.Extra ( firstJust )



-- | flag-description monoid. You probably won't need to use the constructor;
-- mzero or any (<>) of flag(Help|Default) works well.
data Flag p = Flag
  { _flag_help    :: Maybe PP.Doc
  , _flag_default :: Maybe p
  }

instance Monoid (Flag p) where
  mempty = Flag Nothing Nothing
  Flag a1 b1 `mappend` Flag a2 b2 = Flag (a1 <|> a2) (b1 <|> b2)

-- | Create a 'Flag' with just a help text.
flagHelp :: PP.Doc -> Flag p
flagHelp h = mempty { _flag_help = Just h }

-- | Create a 'Flag' with just a help text.
flagHelpStr :: String -> Flag p
flagHelpStr s = mempty { _flag_help = Just $ PP.text s }

-- | Create a 'Flag' with just a default value.
flagDefault :: p -> Flag p
flagDefault d = mempty { _flag_default = Just d }

-- | A no-parameter flag where non-occurence means False, occurence means True.
addSimpleBoolFlag
  :: Applicative f
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, e.g. ["verbose"]
  -> Flag Void -- ^ properties
  -> CmdParser f out Bool
addSimpleBoolFlag shorts longs flag =
  addSimpleBoolFlagAll shorts longs flag (pure ())

-- | Applicative-enabled version of 'addSimpleFlag'
addSimpleFlagA
  :: String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, e.g. ["verbose"]
  -> Flag Void -- ^ properties
  -> f () -- ^ action to execute whenever this matches
  -> CmdParser f out ()
addSimpleFlagA shorts longs flag act
  = void $ addSimpleBoolFlagAll shorts longs flag act

addSimpleBoolFlagAll
  :: String
  -> [String]
  -> Flag Void
  -> f ()
  -> CmdParser f out Bool
addSimpleBoolFlagAll shorts longs flag a
  = fmap (not . null)
  $ addCmdPartManyA ManyUpperBound1 desc parseF (\() -> a)
  where
    allStrs = fmap (\c -> "-"++[c]) shorts
           ++ fmap (\s -> "--"++s)  longs
    desc :: PartDesc
    desc = (maybe id PartWithHelp $ _flag_help flag)
         $ PartAlts $ PartLiteral <$> allStrs
    parseF :: String -> Maybe ((), String)
    parseF str = ( firstJust (\s -> [ ((), drop (length s) str) | s==str ])
                             allStrs)
             <|> ( firstJust (\s -> [ ((), drop (length s + 1) str)
                                    | (s ++ " ") `isPrefixOf` str ])
                             allStrs)

-- | A no-parameter flag that can occur multiple times. Returns the number of
-- occurences (0 or more).
addSimpleCountFlag :: Applicative f
                   => String -- ^ short flag chars, i.e. "v" for -v
                   -> [String] -- ^ list of long names, i.e. ["verbose"]
                   -> Flag Void -- ^ properties
                   -> CmdParser f out Int
addSimpleCountFlag shorts longs flag
  = fmap length
  $ addCmdPartMany ManyUpperBoundN desc parseF
  where
    -- we _could_ allow this to parse repeated short flags, like "-vvv"
    -- (meaning "-v -v -v") correctly.
    allStrs = fmap (\c -> "-"++[c]) shorts
           ++ fmap (\s -> "--"++s)  longs
    desc :: PartDesc
    desc = (maybe id PartWithHelp $ _flag_help flag)
         $ PartAlts $ PartLiteral <$> allStrs
    parseF :: String -> Maybe ((), String)
    parseF str = ( firstJust (\s -> [ ((), drop (length s) str) | s==str ])
                             allStrs)
             <|> ( firstJust (\s -> [ ((), drop (length s + 1) str)
                                    | (s ++ " ") `isPrefixOf` str ])
                             allStrs)

-- | One-argument flag, where the argument is parsed via its Read instance.
addFlagReadParam
  :: forall f p out
   . (Applicative f, Typeable p, Text.Read.Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> CmdParser f out p
addFlagReadParam shorts longs name flag = addCmdPartInpA desc parseF (\_ -> pure ())
  where
    allStrs = fmap (\c -> "-"++[c]) shorts
           ++ fmap (\s -> "--"++s)  longs
    desc = (maybe id PartWithHelp $ _flag_help flag)
         $ maybe id (PartDefault . show) (_flag_default flag)
         $ PartSeq [desc1, desc2]
    desc1 :: PartDesc
    desc1 = PartAlts $ PartLiteral <$> allStrs
    desc2 = PartVariable name
    parseF :: Input -> Maybe (p, Input)
    parseF inp = case inp of
      InputString str -> case flip firstJust allStrs $ \s ->
          [drop (length s + 1) str | (s ++ " ") `isPrefixOf` str]
        of
          Nothing -> _flag_default flag <&> \x -> (x, InputString str)
          Just rest -> case Text.Read.reads rest of
            ((x, ' ':r):_) -> Just (x, InputString $ dropWhile Char.isSpace r)
            ((x, ""):_)    -> Just (x, InputString $ "")
            _ -> Nothing
      InputArgs (arg1:argR) | any (==arg1) allStrs -> case argR of
        [] -> Nothing
        (arg2:rest) -> readMaybe arg2 <&> \x -> (x, InputArgs rest)
      InputArgs _ -> _flag_default flag <&> \d -> (d, inp)

-- | One-argument flag, where the argument is parsed via its Read instance.
-- This version can accumulate multiple values by using the same flag with
-- different arguments multiple times.
--
-- E.g. "--foo 3 --foo 5" yields [3,5].
addFlagReadParams
  :: forall f p out
   . (Applicative f, Typeable p, Text.Read.Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> CmdParser f out [p]
addFlagReadParams shorts longs name flag
  = addFlagReadParamsAll shorts longs name flag (\_ -> pure ())

-- TODO: this implementation is wrong, because it uses addCmdPartManyInpA
--       while this really is no Many.
-- | Applicative-enabled version of 'addFlagReadParam'
-- addFlagReadParamA
--   :: forall f p out
--    . (Typeable p, Text.Read.Read p, Show p)
--   => String -- ^ short flag chars, i.e. "v" for -v
--   -> [String] -- ^ list of long names, i.e. ["verbose"]
--   -> String -- ^ param name
--   -> Flag p -- ^ properties
--   -> (p -> f ()) -- ^ action to execute when ths param matches
--   -> CmdParser f out ()
-- addFlagReadParamA shorts longs name flag act
--   = void $ addFlagReadParamsAll shorts longs name flag act

addFlagReadParamsAll
  :: forall f p out
   . (Typeable p, Text.Read.Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> (p -> f ()) -- ^ action to execute when ths param matches
  -> CmdParser f out [p]
addFlagReadParamsAll shorts longs name flag act =
  addCmdPartManyInpA ManyUpperBoundN desc parseF act
  where
    allStrs = fmap (\c -> "-"++[c]) shorts
           ++ fmap (\s -> "--"++s)  longs
    desc = (maybe id PartWithHelp $ _flag_help flag)
         $ PartSeq [desc1, desc2]
    desc1 :: PartDesc
    desc1 = PartAlts $ PartLiteral <$> allStrs
    desc2 = (maybe id (PartDefault . show) $ _flag_default flag)
          $ PartVariable name
    parseF :: Input -> Maybe (p, Input)
    parseF inp = case inp of
      InputString str -> flip firstJust allStrs $ \s ->
        [ t
        | (s ++ " ") `isPrefixOf` str
        , let rest = drop (length s + 1) str
        , t <- case Text.Read.reads rest of
           ((x, ' ':r):_) -> Just (x, InputString $ dropWhile Char.isSpace r)
           ((x, ""):_)    -> Just (x, InputString $ "")
           _ -> _flag_default flag <&> \x -> (x, InputString rest)
        ]
      InputArgs (arg1:argR) | any (==arg1) allStrs -> case argR of
        [] -> _flag_default flag <&> \d -> (d, InputArgs argR)
        (arg2:rest) -> case readMaybe arg2 of
          Just x -> Just (x, InputArgs rest)
          Nothing -> _flag_default flag <&> \d -> (d, InputArgs argR)
      InputArgs _ -> Nothing


-- | One-argument flag where the argument can be an arbitrary string.
addFlagStringParam
  :: forall f out
   . (Applicative f)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag String -- ^ properties
  -> CmdParser f out String
addFlagStringParam shorts longs name flag = addCmdPartInpA desc parseF (\_ -> pure ())
  where
    allStrs = fmap (\c -> "-"++[c]) shorts
           ++ fmap (\s -> "--"++s)  longs
    desc = (maybe id PartWithHelp $ _flag_help flag)
         $ PartSeq [desc1, desc2]
    desc1 :: PartDesc
    desc1 = PartAlts $ PartLiteral <$> allStrs
    desc2 = PartVariable name
    parseF :: Input -> Maybe (String, Input)
    parseF (InputString str) = case flip firstJust allStrs $ \s ->
        [drop (length s + 1) str | (s ++ " ") `isPrefixOf` str]
      of
        Nothing -> _flag_default flag <&> \x -> (x, InputString str)
        Just rest1 -> let (x, rest2) = break Char.isSpace $ dropWhile Char.isSpace rest1
                      in Just (x, InputString rest2)
    parseF (InputArgs (s1:s2:sr)) | any (==s1) allStrs = Just (s2, InputArgs sr)
    parseF inp@(InputArgs _) = _flag_default flag <&> \x -> (x, inp)

-- | One-argument flag where the argument can be an arbitrary string.
-- This version can accumulate multiple values by using the same flag with
-- different arguments multiple times.
--
-- E.g. "--foo abc --foo def" yields ["abc", "def"].
addFlagStringParams
  :: forall f out
   . (Applicative f)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag Void -- ^ properties
  -> CmdParser f out [String]
addFlagStringParams shorts longs name flag
  = addFlagStringParamsAll shorts longs name flag (\_ -> pure ())

-- TODO: this implementation is wrong, because it uses addCmdPartManyInpA
--       while this really is no Many.
-- -- | Applicative-enabled version of 'addFlagStringParam'
-- addFlagStringParamA
--   :: forall f out
--   .  String -- ^ short flag chars, i.e. "v" for -v
--   -> [String] -- ^ list of long names, i.e. ["verbose"]
--   -> String -- ^ param name
--   -> Flag Void -- ^ properties
--   -> (String -> f ()) -- ^ action to execute when ths param matches
--   -> CmdParser f out ()
-- addFlagStringParamA shorts longs name flag act
--   = void $ addFlagStringParamsAll shorts longs name flag act

addFlagStringParamsAll
  :: forall f out
   . String
  -> [String]
  -> String
  -> Flag Void -- we forbid the default because it has bad interaction
               -- with the eat-anything behaviour of the string parser.
  -> (String -> f ())
  -> CmdParser f out [String]
addFlagStringParamsAll shorts longs name flag act =
  addCmdPartManyInpA ManyUpperBoundN desc parseF act
  where
    allStrs = fmap (\c -> "-"++[c]) shorts
           ++ fmap (\s -> "--"++s)  longs
    desc = (maybe id PartWithHelp $ _flag_help flag)
         $ PartSeq [desc1, desc2]
    desc1 :: PartDesc
    desc1 = PartAlts $ PartLiteral <$> allStrs
    desc2 = (maybe id (PartDefault . show) $ _flag_default flag)
          $ PartVariable name
    parseF :: Input -> Maybe (String, Input)
    parseF (InputString str)
      = flip firstJust allStrs
      $ \s -> [ (x, InputString rest2)
              | (s ++ " ") `isPrefixOf` str
              , let rest1 = drop (length s + 1) str
              , let (x, rest2) = break Char.isSpace $ dropWhile Char.isSpace rest1
              ]
    parseF (InputArgs (s1:s2:sr))
      = flip firstJust allStrs
      $ \s -> [ (s2, InputArgs sr)
              | s == s1
              ]
    parseF (InputArgs _) = Nothing
