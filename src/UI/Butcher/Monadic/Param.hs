
-- | Parameters are arguments of your current command that are not prefixed
-- by some flag. Typical commandline interface is something like
-- "PROGRAM [FLAGS] INPUT". Here, FLAGS are Flags in butcher, and INPUT is
-- a Param, in this case a String representing a path, for example.
module UI.Butcher.Monadic.Param
  ( Param(..)
  , paramHelp
  , paramHelpStr
  , paramDefault
  , paramSuggestions
  , paramFile
  , paramDirectory
  , addParamRead
  , addParamReadOpt
  , addParamString
  , addParamStringOpt
  , addParamStrings
  , addParamNoFlagString
  , addParamNoFlagStringOpt
  , addParamNoFlagStrings
  , addParamRestOfInput
  , addParamRestOfInputRaw
  , -- * Deprecated for more consistent naming
    addReadParam
  , addReadParamOpt
  , addStringParam
  , addStringParamOpt
  , addStringParams
  , addRestOfInputStringParam
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



-- | param-description monoid. You probably won't need to use the constructor;
-- mzero or any (<>) of param(Help|Default|Suggestion) works well.
data Param p = Param
  { _param_default :: Maybe p
  , _param_help :: Maybe PP.Doc
  , _param_suggestions :: Maybe [CompletionItem]
  }

appendParam :: Param p -> Param p -> Param p
appendParam (Param a1 b1 c1) (Param a2 b2 c2) = Param (a1 `f` a2)
                                                      (b1 `mappend` b2)
                                                      (c1 `mappend` c2)
 where
  f Nothing x = x
  f x       _ = x

instance Semigroup (Param p) where
  (<>) = appendParam

instance Monoid (Param p) where
  mempty = Param Nothing Nothing Nothing
  mappend = appendParam

-- | Create a 'Param' with just a help text.
paramHelpStr :: String -> Param p
paramHelpStr s = mempty { _param_help = Just $ PP.text s }

-- | Create a 'Param' with just a help text.
paramHelp :: PP.Doc -> Param p
paramHelp h = mempty { _param_help = Just h }

-- | Create a 'Param' with just a default value.
paramDefault :: p -> Param p
paramDefault d = mempty { _param_default = Just d }

-- | Create a 'Param' with just a list of suggestion values.
paramSuggestions :: [String] -> Param p
paramSuggestions ss =
  mempty { _param_suggestions = Just $ CompletionString <$> ss }

-- | Create a 'Param' that is a file path.
paramFile :: Param p
paramFile = mempty { _param_suggestions = Just [CompletionFile] }

-- | Create a 'Param' that is a directory path.
paramDirectory :: Param p
paramDirectory = mempty { _param_suggestions = Just [CompletionDirectory] }

-- | Add a parameter to the 'CmdParser' by making use of a 'Text.Read.Read'
-- instance. Take care not to use this to return Strings unless you really
-- want that, because it will require the quotation marks and escaping as
-- is normal for the Show/Read instances for String.
addParamRead :: forall f out a
              . (Applicative f, Typeable a, Show a, Text.Read.Read a)
             => String -- ^ paramater name, for use in usage/help texts
             -> Param a -- ^ properties
             -> CmdParser f out a
addParamRead = addReadParam
{-# DEPRECATED addReadParam "use 'addParamRead'" #-}
addReadParam :: forall f out a
              . (Applicative f, Typeable a, Show a, Text.Read.Read a)
             => String -- ^ paramater name, for use in usage/help texts
             -> Param a -- ^ properties
             -> CmdParser f out a
addReadParam name par = addCmdPart desc parseF
  where
    desc :: PartDesc
    desc = addSuggestion (_param_suggestions par)
         $ (maybe id PartWithHelp $ _param_help par)
         $ (maybe id (PartDefault . show) $ _param_default par)
         $ PartVariable name
    parseF :: String -> Maybe (a, String)
    parseF s = case Text.Read.reads s of
      ((x, ' ':r):_) -> Just (x, dropWhile Char.isSpace r)
      ((x, []):_)    -> Just (x, [])
      _ -> _param_default par <&> \x -> (x, s)

-- | Like addReadParam, but optional. I.e. if reading fails, returns Nothing.
addParamReadOpt :: forall f out a
                 . (Applicative f, Typeable a, Text.Read.Read a)
                => String -- ^ paramater name, for use in usage/help texts
                -> Param a -- ^ properties
                -> CmdParser f out (Maybe a)
addParamReadOpt = addReadParamOpt
{-# DEPRECATED addReadParamOpt "use 'addParamReadOpt'" #-}
addReadParamOpt :: forall f out a
                 . (Applicative f, Typeable a, Text.Read.Read a)
                => String -- ^ paramater name, for use in usage/help texts
                -> Param a -- ^ properties
                -> CmdParser f out (Maybe a)
addReadParamOpt name par = addCmdPart desc parseF
  where
    desc :: PartDesc
    desc = addSuggestion (_param_suggestions par)
         $ PartOptional
         $ (maybe id PartWithHelp $ _param_help par)
         $ PartVariable name
    parseF :: String -> Maybe (Maybe a, String)
    parseF s = case Text.Read.reads s of
      ((x, ' ':r):_) -> Just (Just x, dropWhile Char.isSpace r)
      ((x, []):_)    -> Just (Just x, [])
      _ -> Just (Nothing, s) -- TODO: we could warn about a default..

-- | Add a parameter that matches any string of non-space characters if
-- input==String, or one full argument if input==[String]. See the 'Input' doc
-- for this distinction.
addParamString
  :: forall f out . (Applicative f)
  => String
  -> Param String
  -> CmdParser f out String
addParamString = addStringParam
{-# DEPRECATED addStringParam "use 'addParamString'" #-}
addStringParam
  :: forall f out . (Applicative f)
  => String
  -> Param String
  -> CmdParser f out String
addStringParam name par = addCmdPartInp desc parseF
  where
    desc :: PartDesc
    desc = addSuggestion (_param_suggestions par)
         $ (maybe id PartWithHelp $ _param_help par)
         $ PartVariable name
    parseF :: Input -> Maybe (String, Input)
    parseF (InputString str)
      = case break Char.isSpace $ dropWhile Char.isSpace str of
          ("", rest) -> _param_default par <&> \x -> (x, InputString rest)
          (x, rest) -> Just (x, InputString rest)
    parseF (InputArgs args) = case args of
      (s1:sR) -> Just (s1, InputArgs sR)
      []      -> _param_default par <&> \x -> (x, InputArgs args)

-- | Like 'addParamString', but optional, I.e. succeeding with Nothing if
-- there is no remaining input.
addParamStringOpt
  :: forall f out . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out (Maybe String)
addParamStringOpt = addStringParamOpt
{-# DEPRECATED addStringParamOpt "use 'addParamStringOpt'" #-}
addStringParamOpt
  :: forall f out . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out (Maybe String)
addStringParamOpt name par = addCmdPartInp desc parseF
  where
    desc :: PartDesc
    desc = addSuggestion (_param_suggestions par)
         $ PartOptional
         $ (maybe id PartWithHelp $ _param_help par)
         $ PartVariable name
    parseF :: Input -> Maybe (Maybe String, Input)
    parseF (InputString str)
      = case break Char.isSpace $ dropWhile Char.isSpace str of
          ("", rest) -> Just (Nothing, InputString rest)
          (x, rest) -> Just (Just x, InputString rest)
    parseF (InputArgs args) = case args of
      (s1:sR) -> Just (Just s1, InputArgs sR)
      []      -> Just (Nothing, InputArgs [])


-- | Add a parameter that matches any string of non-space characters if
-- input==String, or one full argument if input==[String]. See the 'Input' doc
-- for this distinction.
addParamStrings
  :: forall f out
   . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out [String]
addParamStrings = addStringParams
{-# DEPRECATED addStringParams "use 'addParamStrings'" #-}
addStringParams
  :: forall f out
   . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out [String]
addStringParams name par = addCmdPartManyInp ManyUpperBoundN desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> Maybe (String, Input)
  parseF (InputString str) =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      ("", _   ) -> Nothing
      (x , rest) -> Just (x, InputString rest)
  parseF (InputArgs args) = case args of
    (s1:sR) -> Just (s1, InputArgs sR)
    []      -> Nothing


-- | Like 'addParamString' but does not match strings starting with a dash.
-- This prevents misinterpretation of flags as params.
addParamNoFlagString
  :: forall f out . (Applicative f)
  => String
  -> Param String
  -> CmdParser f out String
addParamNoFlagString name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> Maybe (String, Input)
  parseF (InputString str) =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      (""   , rest) -> _param_default par <&> \x -> (x, InputString rest)
      ('-':_, _   ) -> _param_default par <&> \x -> (x, InputString str)
      (x    , rest) -> Just (x, InputString rest)
  parseF (InputArgs args) = case args of
    []           -> _param_default par <&> \x -> (x, InputArgs args)
    (('-':_):_ ) -> _param_default par <&> \x -> (x, InputArgs args)
    (s1     :sR) -> Just (s1, InputArgs sR)

-- | Like 'addParamStringOpt' but does not match strings starting with a dash.
-- This prevents misinterpretation of flags as params.
addParamNoFlagStringOpt
  :: forall f out
   . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out (Maybe String)
addParamNoFlagStringOpt name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    PartOptional $ (maybe id PartWithHelp $ _param_help par) $ PartVariable name
  parseF :: Input -> Maybe (Maybe String, Input)
  parseF (InputString str) =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      (""   , rest) -> Just (Nothing, InputString rest)
      ('-':_, _   ) -> Just (Nothing, InputString str)
      (x    , rest) -> Just (Just x, InputString rest)
  parseF (InputArgs args) = case args of
    []           -> Just (Nothing, InputArgs [])
    (('-':_):_ ) -> Just (Nothing, InputArgs args)
    (s1     :sR) -> Just (Just s1, InputArgs sR)

-- | Like 'addParamStrings' but does not match strings starting with a dash.
-- This prevents misinterpretation of flags as params.
addParamNoFlagStrings
  :: forall f out
   . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out [String]
addParamNoFlagStrings name par = addCmdPartManyInp ManyUpperBoundN desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> Maybe (String, Input)
  parseF (InputString str) =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      (""   , _   ) -> Nothing
      ('-':_, _   ) -> Nothing
      (x    , rest) -> Just (x, InputString rest)
  parseF (InputArgs args) = case args of
    []           -> Nothing
    (('-':_):_ ) -> Nothing
    (s1     :sR) -> Just (s1, InputArgs sR)


-- | Add a parameter that consumes _all_ remaining input. Typical usecase is
-- after a "--" as common in certain (unix?) commandline tools.
addParamRestOfInput
  :: forall f out . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out String
addParamRestOfInput = addRestOfInputStringParam
{-# DEPRECATED addRestOfInputStringParam "use 'addParamRestOfInput'" #-}
addRestOfInputStringParam
  :: forall f out
   . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out String
addRestOfInputStringParam name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> Maybe (String, Input)
  parseF (InputString str ) = Just (str, InputString "")
  parseF (InputArgs   args) = Just (List.unwords args, InputArgs [])


-- | Add a parameter that consumes _all_ remaining input, returning a raw
-- 'Input' value.
addParamRestOfInputRaw
  :: forall f out . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out Input
addParamRestOfInputRaw name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> Maybe (Input, Input)
  parseF i@InputString{} = Just (i, InputString "")
  parseF i@InputArgs{}   = Just (i, InputArgs [])

