
module Dusk (
   -- Control.Category
   Category (id, (.)),
   (<<<), (>>>),

   -- Data.Functor
   Functor (fmap),

   -- Data.Foldable
   module Data.Foldable,

   -- Data.Traversable
   module Data.Traversable,

   -- Control.Applicative
   Applicative (pure, (<*>), (*>), (<*)),
   Alternative ((<|>), some, many),
   (<$>), (<$), (<**>), liftA, liftA2, liftA3, optional,

   -- Control.Monad
   Monad ((>>=), (>>), return, fail),
   MonadPlus (mzero, mplus),
   (=<<), (>=>), (<=<), forever, void,
   join, mfilter, replicateM, replicateM_,
   guard, when, unless,
   liftM, liftM2, liftM3, liftM4, liftM5, ap,

   -- Control.Monad.Identity
   Identity, runIdentity,

   -- Control.Arrow
   Arrow (arr, first, second, (***), (&&&)),
   returnA, (^>>), (>>^), (<<^), (^<<),
   ArrowZero (zeroArrow),
   ArrowPlus ((<+>)),
   ArrowChoice (left, right, (+++), (|||)),
   ArrowApply (app), leftApp,
   ArrowLoop (loop),

   -- Data.Monoid
   Monoid (mempty, mappend, mconcat), (<>),

   -- Data.Bool
   Bool (False, True), (&&), (||), not, otherwise,

   -- Data.Either
   Either(Left, Right), either, lefts, rights, partitionEithers,

   -- Data.Eq,
   Eq ((==), (/=)),

   -- Data.Function
   const, flip, ($), fix, on,

   -- Data.Maybe
   Maybe (Nothing, Just),
   maybe, isJust, isNothing, fromJust, fromMaybe, listToMaybe, maybeToList, catMaybes, mapMaybe,

   -- Data.List
   module Data.List,

   -- Data.Tuple
   fst, snd, curry, uncurry, swap,

   -- Data.Ord
   Ord (compare, (<), (>=), (>), (<=), max, min),
   Ordering (LT, GT, EQ),
   Down (Down),
   comparing,

   -- Data.Word
   Word8, Word16, Word32, Word64,

   -- Data.Sequence
   Seq,

   -- Data.ByteString
   ByteString, LByteString,

   -- Data.Text
   Text, LText,

   -- Data.String
   String, IsString (fromString),

   -- transformers
   MonadIO (liftIO), lift,

   -- Data.Typeable
   Typeable,

   -- Control.Monad.Catch
   Exception (toException, fromException),
   SomeException,
   IOException,
   MonadCatch,
   throwIO, try, catch, handle, bracket, onException, finally, catchAll,

   -- Debug.Trace
   trace, traceShow,

   -- Text.Show
   Show (show),

   -- Text.Read
   Read, read,

   -- Data.Int
   Int, Int8, Int16, Int32, Int64,

   -- Prelude
   Char, IO, Integer, Float, Double, Rational,
   seq, ($!), undefined,
   Enum (succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo),
   Bounded (minBound, maxBound),
   Num ((+), (*), (-), negate, abs, signum, fromInteger),
   Real (toRational),
   Integral (quot, rem, div, mod, quotRem, divMod, toInteger),
   Fractional ((/), recip, fromRational),
   Floating (pi, exp, sqrt, log, (**), logBase, sin, tan, cos, asin, atan,
      acos, sinh, tanh, cosh, asinh, atanh, acosh),
   RealFrac (properFraction, truncate, round, ceiling, floor),
   RealFloat (floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent, significand,
      scaleFloat, isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE, atan2),
   error,

   fromIntegral, realToFrac,
   subtract, even, odd, gcd, lcm, (^), (^^),

   -- System.IO
   print,

   -- Extras
   tshow, tread, terror,
   (++),
) where

-- exported modules
import Control.Category (Category (id, (.)), (<<<), (>>>))
import Data.Functor (Functor (fmap))
import Data.Foldable
import Data.Traversable
import Control.Applicative (Applicative (pure, (<*>), (*>), (<*)),
   Alternative ((<|>), some, many), (<$>), (<$), (<**>), liftA, liftA2, liftA3, optional)
import Control.Monad (Monad ((>>=), (>>), return, fail),
   MonadPlus (mzero, mplus),
   (=<<), (>=>), (<=<), forever, void,
   join, mfilter, replicateM, replicateM_,
   guard, when, unless,
   liftM, liftM2, liftM3, liftM4, liftM5, ap)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Arrow (Arrow (arr, first, second, (***), (&&&)),
   returnA, (^>>), (>>^), (<<^), (^<<),
   ArrowZero (zeroArrow),
   ArrowPlus ((<+>)),
   ArrowChoice (left, right, (+++), (|||)),
   ArrowApply (app), leftApp,
   ArrowLoop (loop))
import Data.Monoid (Monoid (mempty, mappend, mconcat), (<>))
import Data.Bool (Bool (False, True), (&&), (||), not, otherwise)
import Data.Either (Either(Left, Right), either, lefts, rights, partitionEithers)
import Data.Eq (Eq ((==), (/=)))
import Data.Function (const, flip, ($), fix, on)
import Data.Maybe (Maybe (Nothing, Just),
   maybe, isJust, isNothing, fromJust, fromMaybe, listToMaybe, maybeToList, catMaybes, mapMaybe)
import Data.List (head, last, tail, init, null, length, map, reverse, intersperse, transpose,
   subsequences, permutations, scanl, scanl1, scanr, scanr1, iterate, repeat, replicate, cycle, unfoldr,
   take, drop, splitAt, takeWhile, dropWhile, dropWhileEnd, span, break, stripPrefix, group, inits, tails,
   isPrefixOf, isSuffixOf, isInfixOf, lookup, filter, partition, (!!), elemIndex, elemIndices, findIndex,
   findIndices, zip, zip3, zip4, zip5, zip6, zip7, zipWith, zipWith3, zipWith4, zipWith5, zipWith6, zipWith7,
   unzip, unzip3, unzip4, unzip5, unzip6, unzip7, lines, words, unlines, unwords, nub, delete, (\\), union,
   intersect, sort, insert, nubBy, deleteBy, deleteFirstsBy, unionBy, intersectBy, groupBy, sortBy, insertBy,
   genericLength, genericTake, genericDrop, genericSplitAt, genericIndex, genericReplicate)
import Data.Tuple (fst, snd, curry, uncurry, swap)
import Data.Ord (Ord (compare, (<), (>=), (>), (<=), max, min),
   Ordering (LT, GT, EQ),
   Down (Down),
   comparing)
import Data.Word (Word8, Word16, Word32, Word64)
import Data.Sequence (Seq)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Data.Typeable (Typeable)
--import Control.Exception (Exception (toException, fromException), SomeException, IOException)
--import Control.Exception.Lifted (throwIO, try, catch, handle, bracket, onException, finally)
import Control.Exception (IOException, throwIO)
import Control.Monad.Catch (MonadCatch, Exception (toException, fromException), SomeException,
                            try, catch, handle, bracket, onException, finally, catchAll)
import Debug.Trace (trace, traceShow)
import Text.Show (Show (show))
import Text.Read (Read, read)
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.String (String, IsString (fromString))
import System.IO (print)

import Prelude (Char, IO, seq, ($!), undefined,
   Integer, Float, Double, Rational,
   Enum (succ, pred, toEnum, fromEnum, enumFrom, enumFromThen, enumFromTo, enumFromThenTo),
   Bounded (minBound, maxBound),
   Num ((+), (*), (-), negate, abs, signum, fromInteger),
   Real (toRational),
   Integral (quot, rem, div, mod, quotRem, divMod, toInteger),
   Fractional ((/), recip, fromRational),
   Floating (pi, exp, sqrt, log, (**), logBase, sin, tan, cos, asin,
      atan, acos, sinh, tanh, cosh, asinh, atanh, acosh),
   RealFrac (properFraction, truncate, round, ceiling, floor),
   RealFloat (floatRadix, floatDigits, floatRange, decodeFloat, encodeFloat, exponent,
      significand, scaleFloat, isNaN, isInfinite, isDenormalized, isNegativeZero, isIEEE, atan2),
   fromIntegral, realToFrac, subtract, even, odd, gcd, lcm, (^), (^^))

import qualified Data.Text as T

import qualified Data.ByteString.Lazy
import qualified Data.Text.Lazy
import qualified Prelude

type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString

tshow :: Show s => s -> Text
tshow = T.pack . show

tread :: Read a => Text -> a
tread = read . T.unpack

terror :: Text -> a
terror = Prelude.error . T.unpack

error :: [Char] -> a
error = Prelude.error

(++) :: Monoid m => m -> m -> m
(++) = (<>)
infixr 5 ++ -- same as Prelude.++
