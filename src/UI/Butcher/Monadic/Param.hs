module UI.Butcher.Monadic.Param
  ( Param(..)
  , paramHelp
  , paramHelpStr
  , paramDefault
  , paramSuggestions
  , addReadParam
  , addReadParamOpt
  , addStringParam
  , addStringParamOpt
  , addRestOfInputStringParam
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS

import qualified Text.PrettyPrint as PP

import           Data.HList.ContainsType

import           UI.Butcher.Monadic.Types
import           UI.Butcher.Monadic.Core



data Param p = Param
  { _param_default :: Maybe p
  , _param_help :: Maybe PP.Doc
  , _param_suggestions :: Maybe [p]
  }

instance Monoid (Param p) where
  mempty = Param Nothing Nothing Nothing
  mappend (Param a1 b1 c1)
          (Param a2 b2 c2)
    = Param
          (a1 `f` a2)
          (b1 `mappend` b2)
          (c1 `mappend` c2)
    where
      f Nothing x = x
      f x       _ = x

paramHelpStr :: String -> Param p
paramHelpStr s = mempty { _param_help = Just $ PP.text s }

paramHelp :: PP.Doc -> Param p
paramHelp h = mempty { _param_help = Just h }

paramDefault :: p -> Param p
paramDefault d = mempty { _param_default = Just d }

paramSuggestions :: [p] -> Param p
paramSuggestions ss = mempty { _param_suggestions = Just ss }

addReadParam :: forall f out a
              . (Applicative f, Typeable a, Show a, Text.Read.Read a)
             => String
             -> Param a
             -> CmdParser f out a
addReadParam name par = addCmdPart desc parseF
  where
    desc :: PartDesc
    desc = (maybe id PartWithHelp $ _param_help par)
         $ (maybe id (PartDefault . show) $ _param_default par)
         $ PartVariable name
    parseF :: String -> Maybe (a, String)
    parseF s = case Text.Read.reads s of
      ((x, ' ':r):_) -> Just (x, dropWhile Char.isSpace r)
      ((x, []):_)    -> Just (x, [])
      _ -> _param_default par <&> \x -> (x, s)

addReadParamOpt :: forall f out a
                 . (Applicative f, Typeable a, Text.Read.Read a)
                => String
                -> Param a
                -> CmdParser f out (Maybe a)
addReadParamOpt name par = addCmdPart desc parseF
  where
    desc :: PartDesc
    desc = PartOptional
         $ (maybe id PartWithHelp $ _param_help par)
         $ PartVariable name
    parseF :: String -> Maybe (Maybe a, String)
    parseF s = case Text.Read.reads s of
      ((x, ' ':r):_) -> Just (Just x, dropWhile Char.isSpace r)
      ((x, []):_)    -> Just (Just x, [])
      _ -> Just (Nothing, s) -- TODO: we could warn about a default..

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

addStringParamOpt
  :: forall f out . (Applicative f)
  => String
  -> Param Void
  -> CmdParser f out (Maybe String)
addStringParamOpt name par = addCmdPartInp desc parseF
  where
    desc :: PartDesc
    desc = PartOptional
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
