module UI.Butcher.Monadic.Param
  ( Param(..)
  , paramHelp
  , paramHelpStr
  , paramDefault
  , addReadParam
  , addReadParamOpt
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

import           Data.HList.ContainsType

import           Data.Dynamic

import           UI.Butcher.Monadic.Types
import           UI.Butcher.Monadic.Core



data Param p = Param
  { _param_default :: Maybe p
  , _param_help :: Maybe PP.Doc
  }

instance Monoid (Param p) where
  mempty = Param Nothing Nothing
  Param a1 b1 `mappend` Param a2 b2 = Param (a1 `f` a2) (b1 `mappend` b2)
    where
      f Nothing x = x
      f x       _ = x

paramHelpStr :: String -> Param p
paramHelpStr s = mempty { _param_help = Just $ PP.text s }

paramHelp :: PP.Doc -> Param p
paramHelp h = mempty { _param_help = Just h }

paramDefault :: p -> Param p
paramDefault d = mempty { _param_default = Just d }

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
