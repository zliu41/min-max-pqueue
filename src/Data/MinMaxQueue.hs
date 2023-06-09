{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.MinMaxQueue
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Double-ended priority queues, allowing efficient retrieval and removel
-- from both ends of the queue.
--
-- A queue can be configured with a maximum size. Each time an insertion
-- causes the queue to grow beyond the size limit, the greatest element
-- will be automatically removed (rather than rejecting the insertion).
--
-- If the priority values are 'Int's, use "Data.IntMinMaxQueue".
--
-- The implementation is backed by a @'Map' prio ('NonEmpty' a)@. This means
-- that certain operations, including 'peekMin', 'peekMax' and 'fromList',
-- are asymptotically more expensive than a mutable array based implementation.
-- In a pure language like Haskell, a
-- mutable array based implementation would be impure
-- and need to operate inside monads. And in many applications, regardless
-- of language, the additional time complexity would be a small or negligible
-- price to pay to avoid destructive updates anyway.
--
-- If you only access one end of the queue (i.e., you need a regular
-- priority queue), an implementation based on a kind of heap that is more
-- amenable to purely functional implementations, such as binomial heap
-- and pairing heap, is /potentially/ more efficient. But always benchmark
-- if performance is important; in my experience @Map@ /always/ wins, even for
-- regular priority queues.
--
-- See <https://github.com/zliu41/min-max-pqueue/blob/master/README.md README.md>
-- for more information.
module Data.MinMaxQueue (
  -- * MinMaxQueue type
    MinMaxQueue

  -- * Construction
  , empty
  , singleton
  , fromList
  , fromListWith
  , fromMap

  -- * Size
  , null
  , notNull
  , size

  -- * Maximum size
  , withMaxSize
  , maxSize

  -- * Queue operations
  , insert
  , peekMin
  , peekMax
  , deleteMin
  , deleteMax
  , pollMin
  , pollMax
  , takeMin
  , takeMax
  , dropMin
  , dropMax

  -- * Traversal
  -- ** Map
  , map
  , mapWithPriority

  -- ** Folds
  , foldr
  , foldl
  , foldrWithPriority
  , foldlWithPriority
  , foldMapWithPriority

  -- ** Strict Folds
  , foldr'
  , foldl'
  , foldrWithPriority'
  , foldlWithPriority'

  -- * Lists
  , elems
  , toList
  , toAscList
  , toDescList

  -- * Maps
  , toMap
  ) where

import           Data.Data (Data)
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as Nel

import Prelude hiding (Foldable(..), drop, lookup, map, take)
import qualified Prelude

type Size = Int
type MaxSize = Maybe Int

-- | A double-ended priority queue whose elements are of type @a@ and
-- are compared on @prio@.
data MinMaxQueue prio a = MinMaxQueue {-# UNPACK #-} !Size !MaxSize !(Map prio (NonEmpty a))
  deriving (Eq, Ord, Data)

instance Eq prio => Eq1 (MinMaxQueue prio) where
  liftEq = liftEq2 (==)

instance Eq2 MinMaxQueue where
  liftEq2 eqk eqv q1 q2 =
    Map.size (toMap q1) == Map.size (toMap q2)
      && liftEq (liftEq2 eqk eqv) (toList q1) (toList q2)

instance Ord prio => Ord1 (MinMaxQueue prio) where
  liftCompare = liftCompare2 compare

instance Ord2 MinMaxQueue where
  liftCompare2 cmpk cmpv q1 q2 =
    liftCompare (liftCompare2 cmpk cmpv) (toList q1) (toList q2)

instance (Show prio, Show a) => Show (MinMaxQueue prio a) where
  showsPrec d q = showParen (d > 10) $
    showString "fromList " . shows (toList q)

instance Show prio => Show1 (MinMaxQueue prio) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance Show2 MinMaxQueue where
  liftShowsPrec2 spk slk spv slv d m =
      showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
    where
      sp = liftShowsPrec2 spk slk spv slv
      sl = liftShowList2 spk slk spv slv

instance (Ord prio, Read prio, Read a) => Read (MinMaxQueue prio a) where
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    pure (fromList xs,t)

instance (Ord prio, Read prio) => Read1 (MinMaxQueue prio) where
  liftReadsPrec rp rl = readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance Functor (MinMaxQueue prio) where
  fmap = map

instance Foldable.Foldable (MinMaxQueue prio) where
  foldMap = foldMapWithPriority . const

-- | /O(1)/. The empty queue.
empty :: MinMaxQueue prio a
empty = MinMaxQueue 0 Nothing Map.empty

-- | /O(1)/. A queue with a single element.
singleton :: (a -> prio) -> a -> MinMaxQueue prio a
singleton f a = MinMaxQueue 1 Nothing (Map.singleton (f a) (pure a))

-- | /O(n * log n)/. Build a queue from a list of (priority, element) pairs.
fromList :: Ord prio => [(prio, a)] -> MinMaxQueue prio a
fromList = Foldable.foldr (uncurry (insert . const)) empty

-- | /O(n * log n)/. Build a queue from a list of elements and a function
-- from elements to priorities.
fromListWith :: Ord prio => (a -> prio) -> [a] -> MinMaxQueue prio a
fromListWith f = Foldable.foldr (insert f) empty

-- | /O(n)/ (due to calculating the queue size).
fromMap :: Map prio (NonEmpty a) -> MinMaxQueue prio a
fromMap m = MinMaxQueue (Foldable.sum (fmap Nel.length m)) Nothing m

-- | /O(1)/. Is the queue empty?
null :: MinMaxQueue prio a -> Bool
null = (== 0) . size

-- | /O(1)/. Is the queue non-empty?
notNull :: MinMaxQueue prio a -> Bool
notNull = not . null

-- | /O(1)/. The total number of elements in the queue.
size :: MinMaxQueue prio a -> Int
size (MinMaxQueue sz _ _) = sz

-- | Return a queue that is limited to the given number of elements.
-- If the original queue has more elements than the size limit, the greatest
-- elements will be dropped until the size limit is satisfied.
withMaxSize :: Ord prio => MinMaxQueue prio a -> Int -> MinMaxQueue prio a
withMaxSize q ms = MinMaxQueue sz (Just ms) m
  where (MinMaxQueue sz _ m) = takeMin ms q

-- | /O(1)/. The size limit of the queue. It returns either @Nothing@ (if
-- the queue does not have a size limit) or @Just n@ where @n >= 0@.
maxSize :: MinMaxQueue prio a -> Maybe Int
maxSize (MinMaxQueue _ ms _) = max 0 <$> ms

-- | /O(log n)/. Add the given element to the queue. If the queue has
-- a size limit, and the insertion causes the queue to grow beyond
-- its size limit, the greatest element will be removed from the
-- queue, which may be the element just added.
insert :: Ord prio => (a -> prio) -> a -> MinMaxQueue prio a -> MinMaxQueue prio a
insert f a q@(MinMaxQueue sz ms _) = case ms of
  Just ms' | sz >= ms' -> deleteMax (insert' f a q)
  _ -> insert' f a q

insert' :: Ord prio => (a -> prio) -> a -> MinMaxQueue prio a -> MinMaxQueue prio a
insert' f a (MinMaxQueue sz ms m) = MinMaxQueue (sz+1) ms (Map.alter g (f a) m)
  where
    g Nothing = Just (pure a)
    g (Just as) = Just (a <| as)

-- | /O(log n)/. Retrieve the least element of the queue, if exists.
peekMin :: Ord prio => MinMaxQueue prio a -> Maybe a
peekMin (MinMaxQueue _ _ m) = Nel.head . snd <$> Map.lookupMin m

-- | /O(log n)/. Retrieve the greatest element of the queue, if exists.
peekMax :: Ord prio => MinMaxQueue prio a -> Maybe a
peekMax (MinMaxQueue _ _ m) = Nel.head . snd <$> Map.lookupMax m

-- | /O(log n)/. Remove the least element of the queue, if exists.
deleteMin :: Ord prio => MinMaxQueue prio a -> MinMaxQueue prio a
deleteMin q@(MinMaxQueue sz ms m)
  | Just (prio,_) <- Map.lookupMin m = MinMaxQueue (sz-1) ms (Map.update (Nel.nonEmpty . Nel.tail) prio m)
  | otherwise = q

-- | /O(log n)/. Remove the greatest element of the queue, if exists.
deleteMax :: Ord prio => MinMaxQueue prio a -> MinMaxQueue prio a
deleteMax q@(MinMaxQueue sz ms m)
  | Just (prio,_) <- Map.lookupMax m = MinMaxQueue (sz-1) ms (Map.update (Nel.nonEmpty . Nel.tail) prio m)
  | otherwise = q

-- | /O(log n)/. Remove and return the least element of the queue, if exists.
pollMin :: Ord prio => MinMaxQueue prio a -> Maybe (a, MinMaxQueue prio a)
pollMin q = (,) <$> peekMin q <*> pure (deleteMin q)

-- | /O(log n)/. Remove and return the greatest element of the queue, if exists.
pollMax :: Ord prio => MinMaxQueue prio a -> Maybe (a, MinMaxQueue prio a)
pollMax q = (,) <$> peekMax q <*> pure (deleteMax q)

-- | @'takeMin' n q@ returns a queue with the @n@ least elements in @q@, or
-- @q@ itself if @n >= 'size' q@.
takeMin :: Ord prio => Int -> MinMaxQueue prio a -> MinMaxQueue prio a
takeMin n q@(MinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 <= sz = MinMaxQueue newSz ms (take Map.lookupMin newSz m)
    | otherwise = MinMaxQueue newSz ms (drop Map.lookupMax (sz - newSz) m)
  where newSz = max 0 (min sz n)

-- | @'takeMin' n q@ returns a queue with the @n@ greatest elements in @q@, or
-- @q@ itself if @n >= 'size' q@.
takeMax :: Ord prio => Int -> MinMaxQueue prio a -> MinMaxQueue prio a
takeMax n q@(MinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 <= sz = MinMaxQueue newSz ms (take Map.lookupMax newSz m)
    | otherwise = MinMaxQueue newSz ms (drop Map.lookupMin (sz - newSz) m)
  where newSz = max 0 (min sz n)

-- | @'dropMin' n q@ returns a queue with the @n@ least elements
-- dropped from @q@, or 'empty' if @n >= 'size' q@.
dropMin :: Ord prio => Int -> MinMaxQueue prio a -> MinMaxQueue prio a
dropMin n q@(MinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 > sz = MinMaxQueue newSz ms (drop Map.lookupMin (sz - newSz) m)
    | otherwise = MinMaxQueue newSz ms (take Map.lookupMax newSz m)
  where newSz = max 0 (min sz (sz - n))

-- | @'dropMax' n q@ returns a queue with the @n@ greatest elements
-- dropped from @q@, or 'empty' if @n >= 'size' q@.
dropMax :: Ord prio => Int -> MinMaxQueue prio a -> MinMaxQueue prio a
dropMax n q@(MinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 > sz = MinMaxQueue newSz ms (drop Map.lookupMax (sz - newSz) m)
    | otherwise = MinMaxQueue newSz ms (take Map.lookupMin newSz m)
  where newSz = max 0 (min sz (sz - n))

take
  :: Ord prio
  => (forall b. Map prio b -> Maybe (prio, b))
  -> Int -> Map prio (NonEmpty a) -> Map prio (NonEmpty a)
take lookup n m = go 0 m Map.empty
  where
    go sz mIn mOut
      | sz >= n = mOut
      | Just (prio, hd :| tl) <- lookup mIn =
          let as = hd :| Prelude.take (n - sz - 1) tl
              len = Nel.length as
              mOut' = Map.insert prio as mOut
              mIn' = Map.delete prio mIn
           in go (sz + len) mIn' mOut'
      | otherwise = mOut

drop
  :: Ord prio
  => (forall b. Map prio b -> Maybe (prio, b))
  -> Int -> Map prio (NonEmpty a) -> Map prio (NonEmpty a)
drop lookup n = go 0
  where
    go sz mOut
      | sz >= n = mOut
      | Just (prio, hd :| tl) <- lookup mOut =
          let len = Foldable.length tl + 1
           in if sz + len <= n
                then go (sz + len) (Map.delete prio mOut)
                else Map.insert prio (hd :| Prelude.drop (n - sz) tl) mOut
      | otherwise = mOut

-- | Map a function over all elements in the queue.
map :: (a -> b) -> MinMaxQueue prio a -> MinMaxQueue prio b
map = mapWithPriority . const

-- | Map a function over all elements in the queue.
mapWithPriority :: (prio -> a -> b) -> MinMaxQueue prio a -> MinMaxQueue prio b
mapWithPriority f (MinMaxQueue sz ms m) =
  MinMaxQueue sz ms (Map.mapWithKey (fmap . f) m)

-- | Fold the elements in the queue using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
foldr :: (a -> b -> b) -> b -> MinMaxQueue prio a -> b
foldr = foldrWithPriority . const

-- | Fold the elements in the queue using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
foldl :: (a -> b -> a) -> a -> MinMaxQueue prio b -> a
foldl = foldlWithPriority . (const .)

-- | Fold the elements in the queue using the given right-associative
-- binary operator, such that
-- @'foldrWithPriority' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
foldrWithPriority :: (prio -> a -> b -> b) -> b -> MinMaxQueue prio a -> b
foldrWithPriority f b (MinMaxQueue _ _ m) = Map.foldrWithKey f' b m
  where
    f' = flip . Foldable.foldr . f

-- | Fold the elements in the queue using the given left-associative
-- binary operator, such that
-- @'foldlWithPriority' f z == 'Prelude.foldr' ('uncurry' . f) z . 'toAscList'@.
foldlWithPriority :: (a -> prio -> b -> a) -> a -> MinMaxQueue prio b -> a
foldlWithPriority f a (MinMaxQueue _ _ m) = Map.foldlWithKey f' a m
  where
    f' = flip (Foldable.foldl . flip f)

-- | A strict version of 'foldr'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> MinMaxQueue prio a -> b
foldr' = foldrWithPriority' . const

-- | A strict version of 'foldl'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> MinMaxQueue prio b -> a
foldl' = foldlWithPriority' . (const .)

-- | A strict version of 'foldrWithPriority'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldrWithPriority' :: (prio -> a -> b -> b) -> b -> MinMaxQueue prio a -> b
foldrWithPriority' f b (MinMaxQueue _ _ m) = Map.foldrWithKey' f' b m
  where
    f' = flip . Foldable.foldr . f

-- | A strict version of 'foldlWithPriority'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldlWithPriority' :: (a -> prio -> b -> a) -> a -> MinMaxQueue prio b -> a
foldlWithPriority' f a (MinMaxQueue _ _ m) = Map.foldlWithKey' f' a m
  where
    f' = flip (Foldable.foldl' . flip f)

-- | Fold the elements in the queue using the given monoid, such that
-- @'foldMapWithPriority' f == 'Foldable.foldMap' (uncurry f) . 'elems'@.
foldMapWithPriority :: Monoid m => (prio -> a -> m) -> MinMaxQueue prio a -> m
foldMapWithPriority f (MinMaxQueue _ _ m) =
  Map.foldMapWithKey (Foldable.foldMap . f) m

-- | Elements in the queue in ascending order of priority.
-- Elements with the same priority are returned in no particular order.
elems :: MinMaxQueue prio a -> [a]
elems (MinMaxQueue _ _ m) = Foldable.foldMap Nel.toList m

-- | An alias for 'toAscList'.
toList :: MinMaxQueue prio a -> [(prio, a)]
toList = toAscList

-- | Convert the queue to a list in ascending order of priority.
-- Elements with the same priority are returned in no particular order.
toAscList :: MinMaxQueue prio a -> [(prio, a)]
toAscList (MinMaxQueue _ _ m) =
  Map.toAscList m >>= uncurry (\prio -> fmap (prio,) . Nel.toList)

-- | Convert the queue to a list in descending order of priority.
-- Elements with the same priority are returned in no particular order.
toDescList :: MinMaxQueue prio a -> [(prio, a)]
toDescList (MinMaxQueue _ _ m) =
  Map.toDescList m >>= uncurry (\prio -> fmap (prio,) . Nel.toList)

-- | /O(n)/. Convert the queue to a 'Map'.
toMap :: MinMaxQueue prio a -> Map prio (NonEmpty a)
toMap (MinMaxQueue _ _ m) = m
