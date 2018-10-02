-- this module only re-exports the appropriate user-facing stuff from the
-- internal Types module.
-- | Types used in the butcher interface.
module UI.Butcher.Monadic.Types
  ( CommandDesc(..)
  , cmd_out
  , CmdParser
  , Input (..)
  , ParsingError (..)
  , PartDesc(..)
  , emptyCommandDesc
  , Visibility (..)
  )
where



#include "prelude.inc"



import           UI.Butcher.Monadic.Internal.Types
