
module Dawn.Internal where

import Dusk

-- Enum

-- | Convert one enum to another enum.
enumToEnum :: (Enum a, Enum b) => a -> b
enumToEnum = toEnum . fromEnum

-- Control.Applicative
maybeA :: Alternative f => f a -> f (Maybe a)
maybeA a = Just <$> a <|> pure Nothing

-- Control.Arrow

-- | Arrow.first and Arrow.second composed.
--both :: Arrow a => a b c -> a (b, b) (c, c)
--both f = first f . second f

-- Control.Bool

(<||>) :: Applicative a => a Bool -> a Bool -> a Bool
(<||>) = liftA2 (||)
infixr 2 <||> -- same infix as ||

(<&&>) :: Applicative a => a Bool -> a Bool -> a Bool
(<&&>) = liftA2 (&&)
infixr 3 <&&> -- same infix as &&

-- Control.Monad

(<<) :: Monad m => m b -> m a -> m b
(<<) = flip (>>)
infixl 1 <<

foreverK :: Monad m => (a -> m a) -> (a -> m b)
foreverK k = let r = \a -> k a >>= r in r

bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)

io :: MonadIO m => IO a -> m a
io = liftIO

-- Data.Bool

bool :: a -> a -> Bool -> a
bool f g b = if b then f else g

-- | liftM3 bool
boolM :: Monad m => m a -> m a -> m Bool -> m a
boolM = liftM3 bool

eq :: Eq a => a -> a -> Bool
eq = (==)

neq :: Eq a => a -> a -> Bool
neq = (/=)

-- | less then
lt :: Ord a => a -> a -> Bool
lt = (<)

-- | less then or equal
lte :: Ord a => a -> a -> Bool
lte = (<=)

-- | greater then
gt :: Ord a => a -> a -> Bool
gt = (>)

-- | greater then or equal
gte :: Ord a => a -> a -> Bool
gte = (>=)

-- Data.Either

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

isRight :: Either a b -> Bool
isRight = not . isLeft

fromLeft :: Either a b -> a
fromLeft (Left a) = a
fromLeft _ = error "fromLeft: Right"

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "fromRight: Left"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- Data.Function

-- | Chain two functions, letting the first take two parameters.
(.:.) :: Category cat => cat c d -> (a -> cat b c) -> a -> cat b d
(.:.) = (.) . (.)
infixr 9 .:. -- same infix as (.)
{-# INLINE (.:.) #-}

-- | Chain two functions, letting the first take three parameters.
(.::.) :: Category cat => cat d e -> (a -> b -> cat c d) -> a -> b -> cat c e
(.::.) = (.) . (.) . (.)
infixr 9 .::. -- same infix as (.)
{-# INLINE (.::.) #-}

($$) :: a -> (a -> b) -> b
($$) = flip ($)
infixr 0 $$ -- same infix as $
{-# INLINE ($$) #-}

(.^.) :: (a -> c -> d) -> (b -> c) -> a -> b -> d
(.^.) f g a b = f a (g b)
infixr 9 .^. -- same infix as (.)
{-# INLINE (.^.) #-}

-- Data.Functor

(<$$>) :: Functor f => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
infixl 4 <$$>

($>) :: Functor f => f b -> a -> f a
($>) = flip (<$)
infixl 4 $>

applyF :: Functor f => f (a -> b) -> a -> f b
applyF f a = ($ a) <$> f

applyF' :: Functor f => a -> f (a -> b) -> f b
applyF' = fmap . ($$)

-- Data.Maybe

-- | Test if a parameter passes a test. Nothing it the test fails, Just if it passes.
assess :: (t -> Bool) -> t -> Maybe t
assess f t = if f t then Just t else Nothing

-- Data.Monoid

intercalate :: Monoid s => s -> [s] -> s
intercalate c cs = go cs where
   go [] = mempty
   go [str] = str
   go (str:rest) = str <> c <> go rest

-- Data.Tuple

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

mapFst3 :: (a -> d) -> (a, b, c) -> (d, b, c)
mapFst3 f (a, b, c) = (f a, b, c)

mapSnd3 :: (b -> d) -> (a, b, c) -> (a, d, c)
mapSnd3 f (a, b, c) = (a, f b, c)

mapTrd3 :: (c -> d) -> (a, b, c) -> (a, b, d)
mapTrd3 f (a, b, c) = (a, b, f c)

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

seqTuple :: Monad m => (m a, m b) -> m (a, b)
seqTuple = uncurry $ liftM2 (,)

seqTriple :: Monad m => (m a, m b, m c) -> m (a, b, c)
seqTriple = uncurry3 $ liftM3 (,,)
