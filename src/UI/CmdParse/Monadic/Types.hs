{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonadComprehensions #-}

module UI.CmdParse.Monadic.Types
  ( Command(..)
  , cmd_mParent
  , cmd_help
  , cmd_flags
  , cmd_params
  , cmd_children
  , cmd_run
  , flag_help
  , flag_long
  , flag_params
  , flag_unique
  , flag_short
  , param_def
  , param_help
  , emptyCommand
  , emptyParam
  , FlagParsedMap
  , FlagParsedElement(..)
  , IsParam(..)
  , IsHelpBuilder(..)
  , CmdBuilderF(..)
  , CmdBuilder
  , ParamBuilderF(..)
  , ParamBuilder
  , FlagBuilderF(..)
  , FlagBuilder
  , Flag(..)
  , Param(..)
  , ParamA(..)
  )
where



#include "qprelude/bundle-gamma.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS
import           Data.Unique (Unique)
import qualified System.Unsafe as Unsafe

import qualified Control.Lens.TH as LensTH
import qualified Control.Lens as Lens



data Command out = Command
  { _cmd_mParent  :: Maybe (Command out, String) -- parent command
                                -- , substring that leads to $this.
                                -- (kinda wonky, i know.)
  , _cmd_help     :: Maybe String
  , _cmd_flags    :: [Flag]
  , _cmd_params   :: [ParamA]
  , _cmd_children :: [(String, Command out)]
  , _cmd_run      :: Maybe out
  }

emptyCommand :: Command out
emptyCommand = Command Nothing Nothing [] [] [] Nothing

instance Show (Command out) where
  show c = "Command help=" ++ show (_cmd_help c)
        ++ " flags=" ++ show (_cmd_flags c)
        ++ " params=" ++ show (_cmd_params c)
        ++ " children=" ++ show (_cmd_children c)
        ++ " run=" ++ case _cmd_run c of Nothing -> "Nothing"; Just{} -> "Just{..}"

-- class IsFlag a where
--   flagParse :: String -> Maybe (a, String)
--   staticDef :: a

data Flag = Flag
  { _flag_unique  :: Unique
  , _flag_short   :: String
  , _flag_long    :: [String]
  , _flag_help    :: Maybe String
  , _flag_params  :: [ParamA]
  }

instance Show Flag where
  show (Flag _ short long helpM params) = show (short, long, helpM, params) -- TODO: improve

type FlagParsedMap = Map Unique [FlagParsedElement]

data FlagParsedElement = FlagParsedElement [String]
  deriving Show

data ParamA = forall p . (IsParam p, Show p) => ParamA String (Param p)

deriving instance Show ParamA

class IsParam a where
  paramParse :: String -> Maybe (a, String, String) -- value, representation, rest
  paramStaticDef :: a

data Param a = Param
  { _param_help :: Maybe String
  , _param_def  :: Maybe a
  }

emptyParam :: Param a
emptyParam = Param Nothing Nothing

deriving instance Show a => Show (Param a)

data CmdBuilderF out a
  = CmdBuilderHelp String a
  | forall b . CmdBuilderFlag Unique String [String] (FlagBuilder b) ([b] -> a)
  | forall p . (Show p, IsParam p) => CmdBuilderParam String (ParamBuilder p ()) (p -> a)
  | CmdBuilderChild String (CmdBuilder out ()) a
  | CmdBuilderRun out -- TODO: why do we "abort" here? (i.e. no `a`)
                      -- this is not actually enforced when writing
                      -- CmdBuilders, is it? if it is not, this would result
                      -- in rather nasty silent-ignoring.

deriving instance Functor (CmdBuilderF out)

instance Show a => Show (CmdBuilderF out a) where
  show (CmdBuilderHelp s x) = "(CmdBuilderHelp " ++ show s ++ " " ++ show x ++ ")"
  show (CmdBuilderFlag _ shorts longs _ _) = "(CmdBuilderFlag -" ++ shorts ++ " " ++ show longs ++ ")"
  show (CmdBuilderParam s _ _) = "(CmdBuilderParam " ++ s ++ ")"
  show (CmdBuilderChild s _ _) = "(CmdBuilderChild " ++ s ++ ")"
  show (CmdBuilderRun _) = "CmdBuilderRun"

type CmdBuilder out = Free (CmdBuilderF out)

data FlagBuilderF a
  = FlagBuilderHelp String a
  | forall p . (Show p, IsParam p) => FlagBuilderParam String (ParamBuilder p ()) (p -> a)

deriving instance Functor FlagBuilderF

type FlagBuilder = Free FlagBuilderF

data ParamBuilderF p a
  = ParamBuilderHelp String a
  | ParamBuilderDef p a

deriving instance Functor (ParamBuilderF p)

type ParamBuilder p = Free (ParamBuilderF p)

class IsHelpBuilder m where
  help :: String -> m ()

Lens.makeLenses ''Command
Lens.makeLenses ''Flag
Lens.makeLenses ''Param
