module UI.Butcher.Monadic.Flag
  ( Flag(..)
  , addSimpleBoolFlag
  , addSimpleCountFlag
  , addSimpleFlagA
  , addFlagReadParam
  , addFlagReadParamA
  , addFlagStringParam
  , addFlagStringParamA
  , flagHelp
  , flagHelpStr
  , flagDefault
  )
where



#include "prelude.inc"
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

import           Data.List.Extra ( firstJust )



data Flag p = Flag
  { _flag_help    :: Maybe PP.Doc
  , _flag_default :: Maybe p
  }

instance Monoid (Flag p) where
  mempty = Flag Nothing Nothing
  Flag a1 b1 `mappend` Flag a2 b2 = Flag (a1 <|> a2) (b1 <|> b2)

flagHelp :: PP.Doc -> Flag p
flagHelp h = mempty { _flag_help = Just h }

flagHelpStr :: String -> Flag p
flagHelpStr s = mempty { _flag_help = Just $ PP.text s }

flagDefault :: p -> Flag p
flagDefault d = mempty { _flag_default = Just d }

addSimpleBoolFlag
  :: Applicative f
  => String -> [String] -> Flag Void -> CmdParser f out Bool
addSimpleBoolFlag shorts longs flag =
  addSimpleBoolFlagAll shorts longs flag (pure ())

addSimpleFlagA
  :: String -> [String] -> Flag Void -> f () -> CmdParser f out ()
addSimpleFlagA shorts longs flag act
  = void $ addSimpleBoolFlagAll shorts longs flag act

addSimpleBoolFlagAll
  :: String -- short flag chars, i.e. "v" for -v
  -> [String] -- list of long names, i.e. ["verbose"]
  -> Flag Void
  -> f ()
  -> CmdParser f out Bool
addSimpleBoolFlagAll shorts longs flag a = fmap (not . null)
                                    $ addCmdPartManyA desc parseF (\() -> a)
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

addSimpleCountFlag :: Applicative f
                   => String -- short flag chars, i.e. "v" for -v
                   -> [String] -- list of long names, i.e. ["verbose"]
                   -> Flag Void
                   -> CmdParser f out Int
addSimpleCountFlag shorts longs flag = fmap length
                                     $ addCmdPartMany desc parseF
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

addFlagReadParam
  :: forall f p out
   . (Applicative f, Typeable p, Text.Read.Read p, Show p)
  => String
  -> [String]
  -> String -- param name
  -> Flag p
  -> CmdParser f out [p]
addFlagReadParam shorts longs name flag
  = addFlagReadParamAll shorts longs name flag (\_ -> pure ())

addFlagReadParamA
  :: forall f p out
   . (Typeable p, Text.Read.Read p, Show p)
  => String
  -> [String]
  -> String -- param name
  -> Flag p
  -> (p -> f ())
  -> CmdParser f out ()
addFlagReadParamA shorts longs name flag act
  = void $ addFlagReadParamAll shorts longs name flag act

addFlagReadParamAll
  :: forall f p out
   . (Typeable p, Text.Read.Read p, Show p)
  => String
  -> [String]
  -> String -- param name
  -> Flag p
  -> (p -> f ())
  -> CmdParser f out [p]
addFlagReadParamAll shorts longs name flag act = addCmdPartManyInpA desc parseF act
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

addFlagStringParam
  :: forall f out
   . (Applicative f)
  => String
  -> [String]
  -> String -- param name
  -> Flag Void
  -> CmdParser f out [String]
addFlagStringParam shorts longs name flag
  = addFlagStringParamAll shorts longs name flag (\_ -> pure ())

addFlagStringParamA
  :: forall f out
   . String
  -> [String]
  -> String -- param name
  -> Flag Void
  -> (String -> f ())
  -> CmdParser f out ()
addFlagStringParamA shorts longs name flag act
  = void $ addFlagStringParamAll shorts longs name flag act

addFlagStringParamAll
  :: forall f out
   . String
  -> [String]
  -> String -- param name
  -> Flag Void -- we forbid the default because it has bad interaction
               -- with the eat-anything behaviour of the string parser.
  -> (String -> f ())
  -> CmdParser f out [String]
addFlagStringParamAll shorts longs name flag act = addCmdPartManyInpA desc parseF act
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
              , let (x, rest2) = break (not . Char.isSpace) rest1
              ]
    parseF (InputArgs (s1:s2:sr))
      = flip firstJust allStrs
      $ \s -> [ (s2, InputArgs sr)
              | s == s1
              ]
    parseF (InputArgs _) = Nothing
