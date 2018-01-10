{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Butcher.Monadic.Internal.Types
  ( CommandDesc (..)
  , cmd_mParent
  , cmd_help
  , cmd_synopsis
  , cmd_parts
  , cmd_out
  , cmd_children
  , cmd_visibility
  , emptyCommandDesc
  , CmdParserF (..)
  , CmdParser
  , PartDesc (..)
  , Input (..)
  , ParsingError (..)
  , addSuggestion
  , ManyUpperBound (..)
  , Visibility (..)
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS

import qualified Lens.Micro.TH as LensTH

import qualified Text.PrettyPrint as PP



-- | Butcher supports two input modi: @String@ and @[String]@. Program
-- arguments have the latter form, while parsing interactive command input
-- (e.g. when you implement a terminal of sorts) is easier when you can
-- process the full @String@ without having to wordify it first by some
-- means (and List.words is not the right approach in many situations.)
data Input = InputString String | InputArgs [String]
  deriving (Show, Eq)

-- | Information about an error that occured when trying to parse some @Input@
-- using some @CmdParser@.
data ParsingError = ParsingError
  { _pe_messages  :: [String]
  , _pe_remaining :: Input
  }
  deriving (Show, Eq)

-- | Specifies whether we accept 0-1 or 0-n for @CmdParserPart@s.
data ManyUpperBound
  = ManyUpperBound1
  | ManyUpperBoundN

data Visibility = Visible | Hidden
  deriving (Show, Eq)

data CmdParserF f out a
  =                          CmdParserHelp PP.Doc a
  |                          CmdParserSynopsis String a
  |                          CmdParserPeekDesc (CommandDesc () -> a)
  |                          CmdParserPeekInput (String -> a)
  -- TODO: we can clean up this duplication by providing
  -- a function (String -> Maybe (p, String)) -> (Input -> Maybe (p, Input)).
  | forall p . Typeable p => CmdParserPart PartDesc (String -> Maybe (p, String)) (p -> f ()) (p -> a)
  | forall p . Typeable p => CmdParserPartMany ManyUpperBound PartDesc (String -> Maybe (p, String)) (p -> f ()) ([p] -> a)
  | forall p . Typeable p => CmdParserPartInp PartDesc (Input -> Maybe (p, Input)) (p -> f ()) (p -> a)
  | forall p . Typeable p => CmdParserPartManyInp ManyUpperBound PartDesc (Input -> Maybe (p, Input)) (p -> f ()) ([p] -> a)
  |                          CmdParserChild (Maybe String) Visibility (CmdParser f out ()) (f ()) a
  |                          CmdParserImpl  out                                a
  |                          CmdParserReorderStart                             a
  |                          CmdParserReorderStop                              a
  |                          CmdParserGrouped String                           a
  |                          CmdParserGroupEnd                                 a

-- | The CmdParser monad type. It is a free monad over some functor but users
-- of butcher don't need to know more than that 'CmdParser' is a 'Monad'.
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

-- | A representation/description of a command parser built via the
-- 'CmdParser' monad. Can be transformed into a pretty Doc to display
-- as usage/help via 'UI.Butcher.Monadic.Pretty.ppUsage' and related functions.
--
-- Note that there is the '_cmd_out' accessor that contains @Maybe out@ which
-- might be useful after successful parsing.
data CommandDesc out = CommandDesc
  { _cmd_mParent  :: Maybe (Maybe String, CommandDesc out)
  , _cmd_synopsis :: Maybe PP.Doc
  , _cmd_help     :: Maybe PP.Doc
  , _cmd_parts    :: [PartDesc]
  , _cmd_out      :: Maybe out
  , _cmd_children :: Deque (Maybe String, CommandDesc out)
                     -- we don't use a Map here because we'd like to
                     -- retain the order.
  , _cmd_visibility :: Visibility
  }

-- type PartSeqDesc = [PartDesc]

-- | A representation/description of a command's parts, i.e. flags or params.
-- As a butcher user, the higher-level pretty-printing functions for
-- 'CommandDesc' are probably sufficient.
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
  | PartSuggestion [String] PartDesc
  | PartRedirect String -- name for the redirection
                 PartDesc
  | PartReorder [PartDesc]
  | PartMany PartDesc
  | PartWithHelp PP.Doc PartDesc
  | PartHidden PartDesc
  deriving Show

addSuggestion :: Maybe [String] -> PartDesc -> PartDesc
addSuggestion Nothing     = id
addSuggestion (Just sugs) = PartSuggestion sugs

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
emptyCommandDesc =
  CommandDesc Nothing Nothing Nothing [] Nothing mempty Visible

instance Show (CommandDesc out) where
  show c = "Command help=" ++ show (_cmd_help c)
        ++ " synopsis=" ++ show (_cmd_synopsis c)
        ++ " mParent=" ++ show (fst <$> _cmd_mParent c)
        ++ " out=" ++ maybe "(none)" (\_ -> "(smth)") (_cmd_out c)
        ++ " parts.length=" ++ show (length $ _cmd_parts c)
        ++ " parts=" ++ show (_cmd_parts c)
        ++ " children=" ++ show (fst <$> _cmd_children c)

--

LensTH.makeLenses ''CommandDesc
LensTH.makeLenses ''PartDesc

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

instance Alternative Deque where
  empty = mempty
  (<|>) = Deque.prepend

instance MonadPlus Deque
