{-# LANGUAGE RankNTypes #-}

module Data where

import Control.Applicative

-- Typeclass instances

instance Functor Logic where
  fmap f (Logic a) = Logic $ \b c -> a (b . f) c

instance Applicative Logic where
  pure a  = Logic $ \b c -> b a c
  f <*> a = Logic $ \b c -> unLogic f (\g d -> unLogic a (b . g) d) c

instance Monad Logic where
  return a = Logic $ \b c -> b a c
  m >>= f  = Logic $ \b c -> unLogic m (\a d -> unLogic (f a) b d) c

instance Functor (State s) where
  fmap f (State s) = State $ \x ->
    let (a, y) = s x
    in (f a, y)

instance Applicative (State s) where
  pure a              = State $ \s -> (a, s)
  State s <*> State t = State $ \s0 ->
    let (f, s1) = s s0
        (a, s2) = t s1
    in (f a, s2)

instance Monad (State s) where
  State s >>= f = State $ \s0 ->
    let (a, s1) = s s0
        State sb = f a
    in sb s1


instance Functor Four where
  fmap f (Four a b c d) =
    Four (f a) (f b) (f c) (f d)

instance Foldable Four where
  -- Sequentially apply f in reverse order
  -- foldr f z (Four a b c d) = a `f` (b `f` (c `f` (d `f` z)))
  foldr f z (Four a b c d) =
    f a $ f b $ f c $ f d z

instance Traversable Four where
  traverse f (Four a b c d) =
    liftA3 Four (f a) (f b) (f c) <*> f d

instance Functor Board where
  fmap f (Board (Four f1 f2 f3 f4)) =
    Board (Four (fmap f f1) (fmap f f2) (fmap f f3) (fmap f f4))

-- Please tell me how I can make this look less ugly
instance Foldable Board where
  foldr f z (Board (Four f1 f2 f3 f4)) = foldr f (foldr f (foldr f (foldr f z f4) f3) f2) f1

-- Handy implementation of liftA4 I found
liftA4 :: Applicative f =>
   (a -> b -> c -> d -> e) ->
   f a -> f b -> f c -> f d -> f e
liftA4 f a b c d = f <$> a <*> b <*> c <*> d

instance Traversable Board where
  traverse f (Board (Four f1 f2 f3 f4)) =
    fmap Board (liftA4 Four (traverse f f1) (traverse f f2) (traverse f f3) (traverse f f4))

newtype Logic a
  = Logic { unLogic :: forall r. (a -> r -> r) -> r -> r }

newtype State s a =
  State { runState :: s -> (a, s) }

data Digit
  = D1
  | D2
  | D3
  | D4
  deriving (Eq, Ord, Show)

data Cell
  = Unknown
  | Known Digit
  deriving (Eq, Ord, Show)

-- A row
data Four a
  = Four a a a a
  deriving (Eq, Ord, Show)

newtype Board a
  = Board (Four (Four a))
  deriving (Eq, Ord, Show)

data Hole
  = Concrete Digit
  | Variable Int
  deriving (Eq, Ord, Show)

data Constraint
  = NotEqual Hole Hole
  deriving (Eq, Ord, Show)