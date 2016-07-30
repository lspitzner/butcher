{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonadComprehensions #-}

module UI.Butcher.Monadic.Types
  ( CommandDesc(..)
  , cmd_mParent
  , cmd_help
  , cmd_synopsis
  , cmd_parts
  , cmd_out
  , cmd_children
  , emptyCommandDesc
  , CmdParserF(..)
  , CmdParser
  , PartDesc(..)
  , Input (..)
  , ParsingError (..)
  )
where



#include "qprelude/bundle-gamma.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS
-- import           Data.Unique (Unique)
import qualified System.Unsafe as Unsafe

import qualified Control.Lens.TH as LensTH
import qualified Control.Lens as Lens

import qualified Text.PrettyPrint as PP

import           Data.Dynamic



data Input = InputString String | InputArgs [String]
  deriving (Show, Eq)

data ParsingError = ParsingError
  { _pe_messages  :: [String]
  , _pe_remaining :: Input
  }
  deriving (Show, Eq)

data CmdParserF f out a
  =                          CmdParserHelp PP.Doc a
  |                          CmdParserSynopsis String a
  |                          CmdParserPeekDesc (CommandDesc out -> a)
  -- TODO: we can clean up this duplication by providing
  -- a function (String -> Maybe (p, String)) -> (Input -> Maybe (p, Input)).
  | forall p . Typeable p => CmdParserPart PartDesc (String -> Maybe (p, String)) (p -> f ()) (p -> a)
  | forall p . Typeable p => CmdParserPartMany PartDesc (String -> Maybe (p, String)) (p -> f ()) ([p] -> a)
  | forall p . Typeable p => CmdParserPartInp PartDesc (Input -> Maybe (p, Input)) (p -> f ()) (p -> a)
  | forall p . Typeable p => CmdParserPartManyInp PartDesc (Input -> Maybe (p, Input)) (p -> f ()) ([p] -> a)
  |                          CmdParserChild String (CmdParser f out ()) (f ()) a
  |                          CmdParserImpl  out                                a
  |                          CmdParserReorderStart                             a
  |                          CmdParserReorderStop                              a
  |                          CmdParserGrouped String                           a
  |                          CmdParserGroupEnd                                 a


type CmdParser f out = Free (CmdParserF f out)

-- type CmdParser a = CmdParserM a a

-- data CmdPartParserF a
--   = CmdPartParserHelp String a
--   | forall p . CmdPartParserCore (String -> Maybe (p, String)) -- parser
--                                  (Maybe p) -- optional default value
--                                  (p -> a)
--   | forall p . CmdPartParserOptional (CmdPartParser p)
--                                      (Maybe p -> a)
--   -- the idea here was to allow adding some dynamic data to each "node" of
--   -- the output CommandDesc so the user can potentially add custom additional
--   -- information, and write a custom pretty-printer for e.g. help output
--   -- from that dynamically-enriched CommandDesc structure.
--   -- disabled for now, because i am not sure what exactly "adding to every
--   -- node" involves, because the mapping from Functor to Desc is nontrivial.
--   -- (and because i don't have a direct use-case at the moment..)
--   -- | CmdPartParserCustom Dynamic a
-- 
-- type CmdPartParser = Free CmdPartParserF

---------

data CommandDesc out = CommandDesc
  { _cmd_mParent  :: Maybe (String, CommandDesc out)
  , _cmd_synopsis :: Maybe PP.Doc
  , _cmd_help     :: Maybe PP.Doc
  , _cmd_parts    :: [PartDesc]
  , _cmd_out      :: Maybe out
  , _cmd_children :: [(String, CommandDesc out)]
  }

-- type PartSeqDesc = [PartDesc]

data PartDesc
  = PartLiteral String -- expect a literal string, like "--dry-run"
  | PartVariable String -- expect some user-provided input. The
                               -- string represents the name for the variable
                               -- used in the documentation, e.g. "FILE"
  | PartOptional PartDesc
  | PartAlts [PartDesc]
  | PartSeq [PartDesc]
  | PartDefault String -- default representation
                PartDesc
  | PartRedirect String -- name for the redirection
                 PartDesc
  | PartReorder [PartDesc]
  | PartMany PartDesc
  | PartWithHelp PP.Doc PartDesc
  deriving Show

{-
command documentation structure
1. terminals. e.g. "--dry-run"
2. non-terminals, e.g. "FILES"
3. sequences, e.g. "<program> FLAGS NUMBER PATH"
-- 4. alternatives, e.g. "--date=(relative|local|iso|rfc|..)"
5. sub-commands: git (init|commit|push|clone|..)
   compared to 4, the subcommands have their own flags and params;
   they essentially "take over".
6. optional, e.g. "cabal run [COMPONENT]"
7. default, e.g. "-O(LEVEL=1)"
8. indirection, e.g. "cabal COMMAND\n\nCOMMAND: ..."
-}

--

deriving instance Functor (CmdParserF f out)
deriving instance Functor CommandDesc

--

emptyCommandDesc :: CommandDesc out
emptyCommandDesc = CommandDesc Nothing Nothing Nothing [] Nothing []

instance Show (CommandDesc out) where
  show c = "Command help=" ++ show (_cmd_help c)
        ++ " synopsis=" ++ show (_cmd_synopsis c)
        ++ " mParent=" ++ show (fst <$> _cmd_mParent c)
        ++ " out=" ++ maybe "(none)" (\_ -> "(smth)") (_cmd_out c)
        ++ " parts.length=" ++ show (length $ _cmd_parts c)
        ++ " parts=" ++ show (_cmd_parts c)
        ++ " children=" ++ show (fst <$> _cmd_children c)

--

Lens.makeLenses ''CommandDesc
Lens.makeLenses ''PartDesc

--



-- instance Show FlagDesc where
--   show (FlagDesc _ short long helpM params) = show (short, long, helpM, params) -- TODO: improve

-- class Typeable a => IsParam a where
--   paramParse :: String -> Maybe (a, String, String) -- value, representation, rest
--   paramStaticDef :: a

-- emptyParamDesc :: ParamDesc a
-- emptyParamDesc = ParamDesc Nothing Nothing

-- deriving instance Show a => Show (ParamDesc a)


-- instance Show a => Show (CmdParserF out a) where
--   show (CmdParserHelp s x) = "(CmdParserHelp " ++ show s ++ " " ++ show x ++ ")"
--   show (CmdParserFlag shorts longs _ _) = "(CmdParserFlag -" ++ shorts ++ " " ++ show longs ++ ")"
--   show (CmdParserParam s _ _) = "(CmdParserParam " ++ s ++ ")"
--   show (CmdParserChild s _ _) = "(CmdParserChild " ++ s ++ ")"
--   show (CmdParserRun _) = "CmdParserRun"

