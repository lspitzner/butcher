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

import qualified Data.Either.Combinators

-- import qualified Control.Lens

-- import qualified Control.Monad.Error.Lens
-- import qualified Control.Parallel.Strategies.Lens
-- import qualified Control.Seq.Lens
-- import qualified Data.Array.Lens
-- import qualified Data.Bits.Lens
-- import qualified Data.ByteString.Lazy.Lens
-- import qualified Data.ByteString.Lens
-- import qualified Data.ByteString.Strict.Lens
-- import qualified Data.Complex.Lens
-- import qualified Data.Data.Lens
-- import qualified Data.Dynamic.Lens
-- import qualified Data.HashSet.Lens
-- import qualified Data.IntSet.Lens
-- import qualified Data.List.Lens
-- import qualified Data.Map.Lens
-- import qualified Data.Sequence.Lens
-- import qualified Data.Set.Lens
-- import qualified Data.Text.Lazy.Lens
-- import qualified Data.Text.Lens
-- import qualified Data.Text.Strict.Lens
-- import qualified Data.Tree.Lens
-- import qualified Data.Typeable.Lens
-- import qualified Data.Vector.Generic.Lens
-- import qualified Data.Vector.Lens
-- import qualified GHC.Generics.Lens
-- import qualified Generics.Deriving.Lens
-- import qualified Language.Haskell.TH.Lens
-- import qualified Numeric.Lens
-- import qualified System.Exit.Lens
-- import qualified System.FilePath.Lens
-- import qualified System.IO.Error.Lens

-- import qualified Control.Monad.Cont
-- import qualified Control.Monad.Cont.Class
-- import qualified Control.Monad.Error.Class
-- import qualified Control.Monad.Except
-- import qualified Control.Monad.Identity
-- import qualified Control.Monad.List
-- import qualified Control.Monad.RWS
-- import qualified Control.Monad.RWS.Class
-- import qualified Control.Monad.RWS.Lazy
-- import qualified Control.Monad.RWS.Strict
-- import qualified Control.Monad.Reader
-- import qualified Control.Monad.Reader.Class
-- import qualified Control.Monad.State
-- import qualified Control.Monad.State.Class
-- import qualified Control.Monad.State.Lazy
-- import qualified Control.Monad.State.Strict
-- import qualified Control.Monad.Trans
-- import qualified Control.Monad.Writer
-- import qualified Control.Monad.Writer.Class
-- import qualified Control.Monad.Writer.Lazy
-- import qualified Control.Monad.Writer.Strict

-- import qualified Control.Monad.Trans.MultiRWS
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

-- import qualified Control.Applicative.Backwards
-- import qualified Control.Applicative.Lift
-- import qualified Control.Monad.IO.Class
-- import qualified Control.Monad.Signatures
-- import qualified Control.Monad.Trans.Class
-- import qualified Control.Monad.Trans.Cont
-- import qualified Control.Monad.Trans.Except
-- import qualified Control.Monad.Trans.Identity
-- import qualified Control.Monad.Trans.List
-- import qualified Control.Monad.Trans.Maybe
-- import qualified Control.Monad.Trans.RWS
-- import qualified Control.Monad.Trans.RWS.Lazy
-- import qualified Control.Monad.Trans.RWS.Strict
-- import qualified Control.Monad.Trans.Reader
-- import qualified Control.Monad.Trans.State
-- import qualified Control.Monad.Trans.State.Lazy
-- import qualified Control.Monad.Trans.State.Strict
-- import qualified Control.Monad.Trans.Writer
-- import qualified Control.Monad.Trans.Writer.Lazy
-- import qualified Control.Monad.Trans.Writer.Strict
-- import qualified Data.Functor.Classes
-- import qualified Data.Functor.Compose
-- import qualified Data.Functor.Constant
-- import qualified Data.Functor.Product
-- import qualified Data.Functor.Reverse
-- import qualified Data.Functor.Sum

-- import qualified Prelude
-- import qualified Control.Applicative
-- import qualified Control.Arrow
-- import qualified Control.Category
-- import qualified Control.Concurrent
-- import qualified Control.Concurrent.Chan
-- import qualified Control.Concurrent.MVar
-- import qualified Control.Concurrent.QSem
-- import qualified Control.Concurrent.QSemN
-- import qualified Control.Exception
-- import qualified Control.Exception.Base
-- import qualified Control.Monad
-- import qualified Control.Monad.Fix
-- import qualified Control.Monad.ST
-- import qualified Control.Monad.ST.Lazy
-- import qualified Control.Monad.ST.Lazy.Unsafe
-- import qualified Control.Monad.ST.Strict
-- import qualified Control.Monad.ST.Unsafe
-- import qualified Control.Monad.Zip
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
-- import qualified Data.Ratio
-- import qualified Data.STRef
-- import qualified Data.STRef.Lazy
-- import qualified Data.STRef.Strict
-- import qualified Data.String
-- import qualified Data.Traversable
-- import qualified Data.Tuple
-- import qualified Data.Type.Bool
-- import qualified Data.Type.Coercion
-- import qualified Data.Type.Equality
-- import qualified Data.Typeable
-- import qualified Data.Typeable.Internal
-- import qualified Data.Unique
-- import qualified Data.Version
-- import qualified Data.Void
-- import qualified Data.Word
import qualified Debug.Trace
-- import qualified Foreign.C
-- import qualified Foreign.C.Error
-- import qualified Foreign.C.String
-- import qualified Foreign.C.Types
-- import qualified Foreign.Concurrent
-- import qualified Foreign.ForeignPtr
-- import qualified Foreign.ForeignPtr.Unsafe
-- import qualified Foreign.Marshal
-- import qualified Foreign.Marshal.Alloc
-- import qualified Foreign.Marshal.Array
-- import qualified Foreign.Marshal.Error
-- import qualified Foreign.Marshal.Pool
-- import qualified Foreign.Marshal.Unsafe
-- import qualified Foreign.Marshal.Utils
-- import qualified Foreign.Ptr
-- import qualified Foreign.StablePtr
-- import qualified Foreign.Storable
import qualified Numeric
import qualified Numeric.Natural
-- import qualified System.CPUTime
-- import qualified System.Console.GetOpt
import qualified System.Environment
-- import qualified System.Exit
import qualified System.IO
-- import qualified System.IO.Error
-- import qualified System.IO.Unsafe
-- import qualified System.Info
-- import qualified System.Mem
-- import qualified System.Mem.StableName
-- import qualified System.Mem.Weak
-- import qualified System.Posix.Types
-- import qualified System.Timeout
-- import qualified Text.ParserCombinators.ReadP
-- import qualified Text.ParserCombinators.ReadPrec
-- import qualified Text.Printf
import qualified Text.Read
-- import qualified Text.Read.Lex
import qualified Text.Show
-- import qualified Text.Show.Functions
import qualified Unsafe.Coerce

-- import qualified Control.Arrow as Arrow
-- import qualified Control.Category as Category
-- import qualified Control.Concurrent as Concurrent
-- import qualified Control.Concurrent.Chan as Chan
-- import qualified Control.Concurrent.MVar as MVar
-- import qualified Control.Exception as Exception
-- import qualified Control.Exception.Base as Exception.Base
-- import qualified Control.Monad as Monad
-- import qualified Data.Bits as Bits
import qualified Data.Bool as Bool
import qualified Data.Char as Char
-- import qualified Data.Complex as Complex
-- import qualified Data.Either as Either
-- import qualified Data.Eq as Eq
-- import qualified Data.Foldable as Foldable
-- import qualified Data.Fixed as Fixed
-- import qualified Data.Functor.Identity as Identity
-- import qualified Data.IORef as IORef
-- import qualified Data.Int as Int
-- import qualified Data.Ix as Ix
import qualified Data.Maybe as Maybe
-- import qualified Data.Monoid as Monoid
-- import qualified Data.Ord as Ord
-- import qualified Data.Proxy as Proxy
-- import qualified Data.Traversable as Traversable
-- import qualified Data.Void as Void
import qualified Control.Monad.Trans.Writer.Strict as WriterS

#if MIN_VERSION_base(4,9,0)
import qualified GHC.OldList as List
#else
import qualified Data.List as List
#endif

-- import qualified Text.Printf as Printf

import qualified Data.IntMap as IntMap
-- import qualified Data.IntMap.Lazy as IntMapL
import qualified Data.IntMap.Strict as IntMapS
-- import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
-- import qualified Data.Map.Lazy as MapL
-- import qualified Data.Map.Strict as MapS
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Control.Monad.RWS.Class as RWS.Class
import qualified Control.Monad.Reader.Class as Reader.Class
import qualified Control.Monad.State.Class as State.Class
import qualified Control.Monad.Writer.Class as Writer.Class

-- import qualified Control.Monad.Trans.Class as Trans.Class
-- import qualified Control.Monad.Trans.Maybe as Trans.Maybe
-- import qualified Control.Monad.Trans.RWS as RWS
-- import qualified Control.Monad.Trans.RWS.Lazy as RWSL
-- import qualified Control.Monad.Trans.RWS.Strict as RWSS
-- import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.State.Lazy as StateL
import qualified Control.Monad.Trans.State.Strict as StateS
-- import qualified Control.Monad.Trans.Writer as Writer
-- import qualified Control.Monad.Trans.Writer.Lazy as WriterL
-- import qualified Control.Monad.Trans.Writer.Strict as Writer

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

import Data.Map ( Map )
import Data.Set ( Set )

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

import Data.Monoid                        ( (<>)
                                          , mconcat
                                          , Monoid (..)
                                          )

import Data.Bifunctor                     ( bimap )
import Data.Functor                       ( (<$), ($>) )
import Data.Function                      ( (&) )
import System.IO                          ( hFlush
                                          , stdout
                                          )

import Data.Typeable                      ( Typeable
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

import Control.Monad.Trans.MultiRWS       ( -- MultiRWST (..)
                                          -- , MultiRWSTNull
                                          -- , MultiRWS
                                          -- , 
                                            MonadMultiReader(..)
                                          , MonadMultiWriter(..)
                                          , MonadMultiState(..)
                                          -- , runMultiRWST
                                          -- , runMultiRWSTASW
                                          -- , runMultiRWSTW
                                          -- , runMultiRWSTAW
                                          -- , runMultiRWSTSW
                                          -- , runMultiRWSTNil
                                          -- , runMultiRWSTNil_
                                          -- , withMultiReader
                                          -- , withMultiReader_
                                          -- , withMultiReaders
                                          -- , withMultiReaders_
                                          -- , withMultiWriter
                                          -- , withMultiWriterAW
                                          -- , withMultiWriterWA
                                          -- , withMultiWriterW
                                          -- , withMultiWriters
                                          -- , withMultiWritersAW
                                          -- , withMultiWritersWA
                                          -- , withMultiWritersW
                                          -- , withMultiState
                                          -- , withMultiStateAS
                                          -- , withMultiStateSA
                                          -- , withMultiStateA
                                          -- , withMultiStateS
                                          -- , withMultiState_
                                          -- , withMultiStates
                                          -- , withMultiStatesAS
                                          -- , withMultiStatesSA
                                          -- , withMultiStatesA
                                          -- , withMultiStatesS
                                          -- , withMultiStates_
                                          -- , inflateReader
                                          -- , inflateMultiReader
                                          -- , inflateWriter
                                          -- , inflateMultiWriter
                                          -- , inflateState
                                          -- , inflateMultiState
                                          -- , mapMultiRWST
                                          -- , mGetRawR
                                          -- , mGetRawW
                                          -- , mGetRawS
                                          -- , mPutRawR
                                          -- , mPutRawW
                                          -- , mPutRawS
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
