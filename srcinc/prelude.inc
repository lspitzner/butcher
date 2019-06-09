import qualified Data.Graph
import qualified Data.IntMap
import qualified Data.IntMap.Lazy
import qualified Data.IntMap.Strict
import qualified Data.IntSet
import qualified Data.Map
import qualified Data.Map.Lazy
import qualified Data.Map.Strict
import qualified Data.Sequence
import qualified Data.Set
import qualified Data.Tree

import qualified Control.Concurrent.Extra
import qualified Control.Exception.Extra
import qualified Control.Monad.Extra
import qualified Data.Either.Extra
import qualified Data.IORef.Extra
import qualified Data.List.Extra
import qualified Data.Tuple.Extra
import qualified Data.Version.Extra
import qualified Numeric.Extra
import qualified System.Directory.Extra
import qualified System.Environment.Extra
import qualified System.IO.Extra
import qualified System.Info.Extra
import qualified System.Process.Extra
import qualified System.Time.Extra

import qualified Control.Monad.Trans.MultiRWS.Lazy
import qualified Control.Monad.Trans.MultiRWS.Strict
import qualified Control.Monad.Trans.MultiReader
import qualified Control.Monad.Trans.MultiReader.Class
import qualified Control.Monad.Trans.MultiReader.Lazy
import qualified Control.Monad.Trans.MultiReader.Strict
import qualified Control.Monad.Trans.MultiState
import qualified Control.Monad.Trans.MultiState.Class
import qualified Control.Monad.Trans.MultiState.Lazy
import qualified Control.Monad.Trans.MultiState.Strict
import qualified Control.Monad.Trans.MultiWriter
import qualified Control.Monad.Trans.MultiWriter.Class
import qualified Control.Monad.Trans.MultiWriter.Lazy
import qualified Control.Monad.Trans.MultiWriter.Strict

import qualified Control.Monad.Trans.MultiRWS.Strict as MultiRWSS
import qualified Control.Monad.Trans.MultiRWS.Lazy as MultiRWSL

import qualified Data.Bifunctor
import qualified Data.Bits
import qualified Data.Bool
import qualified Data.Char
import qualified Data.Coerce
import qualified Data.Complex
import qualified Data.Data
import qualified Data.Dynamic
import qualified Data.Either
import qualified Data.Eq
import qualified Data.Fixed
import qualified Data.Foldable
import qualified Data.Function
import qualified Data.Functor
import qualified Data.Functor.Identity
import qualified Data.IORef
import qualified Data.Int
import qualified Data.Ix
import qualified Data.List
import qualified Data.Maybe
import qualified Data.Monoid
import qualified Data.Ord
import qualified Data.Proxy
import qualified Debug.Trace
import qualified Numeric
import qualified Numeric.Natural
import qualified System.Environment
import qualified System.IO
import qualified Text.Read
import qualified Text.Show
import qualified Unsafe.Coerce

import qualified Data.Bool as Bool
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Control.Monad.Trans.Writer.Strict as WriterS

#if MIN_VERSION_base(4,9,0)
import qualified GHC.OldList as List
#else
import qualified Data.List as List
#endif

import qualified Data.IntMap as IntMap
import qualified Data.IntMap.Strict as IntMapS
import qualified Data.Map.Strict as MapS
import qualified Data.Map.Lazy as MapL
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Control.Monad.RWS.Class as RWS.Class
import qualified Control.Monad.Reader.Class as Reader.Class
import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Writer.Class as Writer.Class

import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Trans.State.Strict as StateS

import Data.Functor.Identity ( Identity(..) )
import Control.Concurrent.Chan ( Chan )
import Control.Concurrent.MVar ( MVar )
import Data.Int ( Int )
import Data.Word ( Word )
import Prelude ( Integer, Float, Double )
import Control.Monad.ST ( ST )
import Data.Bool ( Bool(..) )
import Data.Char ( Char )
import Data.Either ( Either(..) )
import Data.IORef ( IORef )
import Data.Maybe ( Maybe(..) )
import Data.Monoid ( Endo(..), All(..), Any(..), Sum(..), Product(..), First(..), Last(..), Alt(..), )
import Data.Ord ( Ordering(..), Down(..) )
import Data.Ratio ( Ratio, Rational )
import Data.String ( String )
import Data.Void ( Void )
import System.IO ( IO )
import Data.Proxy ( Proxy(..) )
import Data.Sequence ( Seq )
import Data.Semigroup ( Semigroup(..) )

import Data.Map ( Map )
import Data.Set ( Set )

import Deque.Lazy ( Deque )

import qualified Deque.Lazy as Deque

import Prelude                            ( Char
                                          , String
                                          , Int
                                          , Integer
                                          , Float
                                          , Double
                                          , Bool (..)
                                          , undefined
                                          , Eq (..)
                                          , Ord (..)
                                          , Enum (..)
                                          , Bounded (..)
                                          , Maybe (..)
                                          , Either (..)
                                          , IO
                                          , (<$>)
                                          , (.)
                                          , ($)
                                          , ($!)
                                          , Num (..)
                                          , Integral (..)
                                          , Fractional (..)
                                          , Floating (..)
                                          , RealFrac (..)
                                          , RealFloat (..)
                                          , fromIntegral
                                          , error
                                          , foldr
                                          , foldl
                                          , foldr1
                                          , id
                                          , map
                                          , subtract
                                          , putStrLn
                                          , putStr
                                          , Show (..)
                                          , print
                                          , fst
                                          , snd
                                          , (++)
                                          , not
                                          , (&&)
                                          , (||)
                                          , curry
                                          , uncurry
                                          , Ordering (..)
                                          , flip
                                          , const
                                          , seq
                                          , reverse
                                          , otherwise
                                          , traverse
                                          , realToFrac
                                          , or
                                          , and
                                          , head
                                          , any
                                          , (^)
                                          , Foldable
                                          , Traversable
                                          )

import Data.Foldable                      ( foldl'
                                          , foldr'
                                          , fold
                                          , asum
                                          )

import Data.List                          ( partition
                                          , null
                                          , elem
                                          , notElem
                                          , minimum
                                          , maximum
                                          , length
                                          , all
                                          , take
                                          , drop
                                          , find
                                          , sum
                                          , zip
                                          , zip3
                                          , zipWith
                                          , repeat
                                          , replicate
                                          , iterate
                                          , nub
                                          , filter
                                          , intersperse
                                          , intercalate
                                          , isSuffixOf
                                          , isPrefixOf
                                          , dropWhile
                                          , takeWhile
                                          , unzip
                                          , break
                                          , transpose
                                          , sortBy
                                          , mapAccumL
                                          , mapAccumR
                                          , uncons
                                          )

import Data.Tuple                         ( swap
                                          )

import Data.Char                          ( ord
                                          , chr
                                          )

import Data.Maybe                         ( fromMaybe
                                          , maybe
                                          , listToMaybe
                                          , maybeToList
                                          , catMaybes
                                          )

import Data.Word                          ( Word32
                                          )

import Data.Ord                           ( comparing
                                          , Down (..)
                                          )

import Data.Either                        ( either
                                          )

import Data.Ratio                         ( Ratio
                                          , (%)
                                          , numerator
                                          , denominator
                                          )

import Text.Read                          ( readMaybe
                                          )

import Control.Monad                      ( Functor (..)
                                          , Monad (..)
                                          , MonadPlus (..)
                                          , mapM
                                          , mapM_
                                          , forM
                                          , forM_
                                          , sequence
                                          , sequence_
                                          , (=<<)
                                          , (>=>)
                                          , (<=<)
                                          , forever
                                          , void
                                          , join
                                          , replicateM
                                          , replicateM_
                                          , guard
                                          , when
                                          , unless
                                          , liftM
                                          , liftM2
                                          , liftM3
                                          , liftM4
                                          , liftM5
                                          , filterM
                                          , (<$!>)
                                          )

import Control.Applicative                ( Applicative (..)
                                          , Alternative (..)
                                          )

import Foreign.Storable                   ( Storable )
import GHC.Exts                           ( Constraint )

import Control.Concurrent                 ( threadDelay
                                          , forkIO
                                          , forkOS
                                          )

import Control.Concurrent.MVar            ( MVar
                                          , newEmptyMVar
                                          , newMVar
                                          , putMVar
                                          , readMVar
                                          , takeMVar
                                          , swapMVar
                                          )

import Control.Exception                  ( evaluate
                                          , bracket
                                          , assert
                                          )

import Debug.Trace                        ( trace
                                          , traceId
                                          , traceShowId
                                          , traceShow
                                          , traceStack
                                          , traceShowId
                                          , traceIO
                                          , traceM
                                          , traceShowM
                                          )

import Foreign.ForeignPtr                 ( ForeignPtr
                                          )

import Data.Monoid                        ( Monoid
                                          , mempty
                                          , mconcat
                                          )

import Data.Bifunctor                     ( bimap )
import Data.Functor                       ( (<$), ($>) )
import Data.Function                      ( (&) )
import System.IO                          ( hFlush
                                          , stdout
                                          )

import Data.Typeable                      ( Typeable
                                          , cast
                                          )

import Control.Arrow                      ( first
                                          , second
                                          , (***)
                                          , (&&&)
                                          , (>>>)
                                          , (<<<)
                                          )

import Data.Functor.Identity              ( Identity (..)
                                          )

import Data.Proxy                         ( Proxy (..)
                                          )

import Data.Version                       ( showVersion
                                          )

import Data.List.Extra                    ( nubOrd
                                          , stripSuffix
                                          )
import Control.Monad.Extra                ( whenM
                                          , unlessM
                                          , ifM
                                          , notM
                                          , orM
                                          , andM
                                          , anyM
                                          , allM
                                          )

import Data.Tree                          ( Tree(..)
                                          )

import Control.Monad.Trans.MultiRWS       ( MonadMultiReader(..)
                                          , MonadMultiWriter(..)
                                          , MonadMultiState(..)
                                          , mGet
                                          )

import Control.Monad.Trans.MultiReader    ( runMultiReaderTNil
                                          , runMultiReaderTNil_
                                          , MultiReaderT (..)
                                          , MultiReader
                                          , MultiReaderTNull
                                          )

import Control.Monad.IO.Class             ( MonadIO (..)
                                          )

import Control.Monad.Trans.Class          ( lift
                                          )
import Control.Monad.Trans.Maybe          ( MaybeT (..)
                                          )
import Lens.Micro                         ( (<&>)
                                          )
