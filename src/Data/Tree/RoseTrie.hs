-- "Data/RoseTrie/RoseTrie.hs" provides the RoseTrie data type, a tree combining
-- properties of a Trie and a RoseTrie.
-- 
-- Copyright (C) 2008-2015  Ramin Honary.
--
-- Dao is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.
-- 
-- Dao is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
-- details.
-- 
-- You should have received a copy of the GNU General Public License along with
-- this program (see the file called "LICENSE"). If not, see the URL:
-- <http://www.gnu.org/licenses/agpl.html>.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | A "trie" based on 'Data.Map.Map' where you can store objects @o@ to an arbitrary path
-- constructed of paths-segments @p@. The idea of the 'RoseTrie' data structure is that it behaves
-- exctly like a 'Data.Map.Map' where the keys of the map are of type @[p]@, but internally, similar
-- paths are merged together to save memory and 'Data.Traversable.traverse' time.
--
-- Because of the way similar paths @[p]@ are merged, when you perform a 'Data.Foldable.foldr',
-- 'mergeWithKey', or 'Data.Traversable.traverse' operation, you have a choice of how to order the
-- objects @o@, with 'DepthFirst' or 'BreadthFirst'. Functions like 'elems' and 'assocs' require an
-- additional 'RunRoseTrie' parameter to decide the ordering of the objects @o@.
--
-- Therefore, this data type instantiates 'Data.Foldable.Foldable' only when
-- it is paired with a 'RunRoseTrie' to determine if the 'Data.Foldable.foldr' will occur in
-- 'DepthFirst' or 'BreadthFirst' order.
module Data.Tree.RoseTrie where

import           Prelude hiding (id, (.), mapM, foldr, foldl, sum, concat)

import           Control.Arrow
import           Control.Applicative
import           Control.Category
import           Control.DeepSeq
import           Control.Monad          hiding (mapM, forM, msum)
import           Control.Monad.Identity hiding (mapM, forM, msum)
import           Control.Monad.State    hiding (mapM, forM, msum)

import           Data.Foldable
import           Data.Lens.Minimal
import           Data.Maybe
import           Data.Monoid
import           Data.Typeable
import qualified Data.Map as M
import           Data.Traversable
import           Data.Word

----------------------------------------------------------------------------------------------------

-- | A 'RoseTrie' is just a @newtype@ around a pair of two elements forming a node, the first being the
-- leaf of the node, and the second being the branches of the node. The leaf may or may not exist,
-- so it is wrapped in a 'Prelude.Maybe' data structure.
--
-- When you associate an object @o@ at a path @[p]@, a walk is performed, with each segment of the
-- path @[p]@ selecting a branch that contains another sub-node. When the path @[p]@ is empty, the
-- walk stops and the object @o@ is placed into the current sub-node.
newtype RoseTrie p o = RoseTrie (Maybe o, M.Map p (RoseTrie p o)) deriving (Eq, Ord, Show, Typeable)

instance Functor (RoseTrie p) where { fmap f (RoseTrie (o, m)) = RoseTrie (fmap f o, fmap (fmap f) m); }

instance (Ord p, Monoid o) => Monoid (Sum (RoseTrie p o)) where
  mempty  = Sum Data.Tree.RoseTrie.empty
  mappend (Sum a) (Sum b) = Sum $ unionWith mappend a b

instance (Ord p, Monoid o) => Monoid (Product (RoseTrie p o)) where
  mempty  = Product Data.Tree.RoseTrie.empty
  mappend (Product a) (Product b) = Product $ intersectionWith mappend a b

instance (NFData a, NFData b) => NFData (RoseTrie a b) where
  rnf (RoseTrie (o, m)) = deepseq o $! deepseq m ()

instance (Ord p, Monad m) => FocusesWith [p] m (RoseTrie p o) (Maybe o) where { focus=path; }

instance Foldable (ReduceRoseTrie p) where
  foldr f b (ReduceRoseTrie control tree) = foldr f b $ elems control tree

instance Ord p => Traversable (ReduceRoseTrie p) where
  traverse f (ReduceRoseTrie control tree) = fmap (ReduceRoseTrie control . fromList) $
    traverse (\ (p, o) -> (,) <$> pure p <*> f o) $ assocs control tree

----------------------------------------------------------------------------------------------------

-- | This data type controls algorithms like 'mergeWithKeyM' where monadic evaluation needs to occur
-- in a certain order. This simple operation code decides whether evaluation of leaves happens
-- before evaluation of sub-'RoseTrie's ('BreadthFirst') or whether evaluation of leaves happens after
-- evaluation of sub-'RoseTrie's ('DepthFirst').
data RunRoseTrie
  = DepthFirst
    -- ^ will have the 'Rule' 'Data.Tree.RoseTrie.Leaf's evaluated such that the longest branches evaluate
    -- first.
  | BreadthFirst
    -- ^ will have the 'Rule' 'Data.Tree.RoseTrie.Leaf's evaluated such that the shortest branches evaluate
    -- first.
  deriving (Eq, Ord, Show, Typeable, Enum, Bounded)

-- | Like 'RunRoseTrie', but pairs the 'RunRoseTrie' value with the 'RoseTrie' data type itself. This is used to
-- instantiate 'Data.Foldable.Foldable' and 'Data.Traversable.Traversable', which means in order to
-- use 'Data.Foldable.foldr' or 'Data.Traversable.traverse', it is first necessary to store the tree
-- in this data type along with the 'RunRoseTrie' operator indicating the order in which the leaf
-- objects @o@ will be retrieved.
data ReduceRoseTrie p o = ReduceRoseTrie{ reduceRoseTrieBy :: RunRoseTrie, getReduced :: RoseTrie p o }
  deriving (Eq, Ord, Show, Typeable)

instance Functor (ReduceRoseTrie p) where
  fmap f (ReduceRoseTrie control tree) = ReduceRoseTrie control $ fmap f tree

----------------------------------------------------------------------------------------------------

treePair :: (Monad m, Ord p) => Lens m (RoseTrie p o) (Maybe o, M.Map p (RoseTrie p o))
treePair = newLens (\ (RoseTrie t) -> t) (\t (RoseTrie _) -> RoseTrie t)

empty :: RoseTrie p o
empty = RoseTrie (Nothing, M.empty)

-- | Since 'RoseTrie' does not directly instantiate 'Data.Monoid.Monoid', it cannot be used with the
-- 'Data.Lens.Minimal.new' function. So the 'newTrie' function is provided which behaves similarly.
-- In other words, this function takes a list of transfomration functions that modify a 'RoseTrie',
-- and starting with an 'empty' 'RoiseTrie', applies each transformation in order to build the
-- 'RoseTrie'.
newRoseTrie :: [RoseTrie p o -> RoseTrie p o] -> RoseTrie p o
newRoseTrie = with Data.Tree.RoseTrie.empty

leaf :: (Monad m, Ord p) => Lens m (RoseTrie p o) (Maybe o)
leaf = treePair >>> tuple0

branches :: (Monad m, Ord p) => Lens m (RoseTrie p o) (M.Map p (RoseTrie p o))
branches = treePair >>> tuple1

-- | This is a focusing lens that focuses on a sub-'RoseTrie' at a given path @[p]@. A function that
-- takes two 'RoseTrie's and returns a 'RoseTrie' is required for performing 'Dao.Lens.alter' or
-- 'Dao.Lens.update' operations. This function is not used on 'Dao.Lens.fetch' operations.
--
-- When 'Dao.Lens.update'ing or 'Dao.Lens.alter'ing, the tree in the focus of the 'Dao.Lens.Lens' is
-- passed as the first (left-hand) argument to the given altering function, and the sub-'RoseTrie'
-- found at the path @[p]@ is passed as the second (right-hand) argument.
--
-- For most purposes, the 'Dao.Lens.alter'ing function you want to use is the 'union' function.
focusSubRoseTrie
  :: (Monad m, Ord p)
  => (RoseTrie p o -> RoseTrie p o -> m (RoseTrie p o)) -> [p] -> Lens m (RoseTrie p o) (RoseTrie p o)
focusSubRoseTrie alt px =
  newLensM
    (\  target@(RoseTrie (_, map)) -> case px of
      []    -> return target
      p:px  -> maybe (return Data.Tree.RoseTrie.empty) (fetch $ focusSubRoseTrie alt px) (M.lookup p map)
    )
    (case px of
      []    -> flip alt
      p:px  -> Data.Lens.Minimal.update $
        branches >>> mapLens p >>> orElse (return Data.Tree.RoseTrie.empty) >>> focusSubRoseTrie alt px
    )

-- | Focuses on an individual leaf at the given path.
path :: (Monad m, Ord p) => [p] -> Lens m (RoseTrie p o) (Maybe o)
path px =
  newLensM
    (\  (RoseTrie (o, map)) -> case px of
      []    -> return o
      p:px  -> maybe (return Nothing) (fetch $ path px) (M.lookup p map)
    )
    (\o (RoseTrie (n, map)) -> case px of
      []    -> return $ RoseTrie (o, map)
      p:px  -> do
        t <- Data.Lens.Minimal.update (path px) o $ fromMaybe Data.Tree.RoseTrie.empty $ M.lookup p map
        return $ RoseTrie (n, M.alter (const $ if Data.Tree.RoseTrie.null t then Nothing else Just t) p map)
    )

-- | This function merges two trees together, given a leaf-merging function that can optionally
-- create or remove leaves based on whether or not leaves exist on the left and right at any given
-- point in the path @[p]@.
--
-- Also required are two 'RoseTrie' functions: a function that can convert the first (left)
-- 'RoseTrie' parameter to a 'RoseTrie' of the resultant type, and a function that can convert the
-- second (right) 'RoseTrie' parameter to a 'RoseTrie' of the resultant type. These functions are
-- used for when leaves exist only on the left 'RoseTrie', or for when leaves only exist on the
-- right 'RoseTrie'.
--
-- The given leaf-merging function is called for every single sub-'RoseTrie' node where the path
-- @[p]@ exists in both the overlay and target 'RoseTrie's. Each sub-'RoseTrie' node may or may not
-- have a 'Leaf'.
--
-- * If the 'RoseTrie' node for the overlay 'RoseTrie' and the target 'RoseTrie' are both without
--   leaves, the merging function is passed 'Prelude.Nothing' as both arguments to the updating
--   function. 
--
-- * If only the target 'RoseTrie' has a 'Leaf', the overlay 'Leaf' as passed with 'Prelude.Just' as
--   the first (left) argument to the updating function, and 'Prelude.Nothing' is passed as the
--   second (right) argument.
--
-- * If only the overlay 'RoseTrie' has a leaf, 'Prelude.Nothing' is passed as the first (left)
--   argument to the merging function, and the overlay 'Leaf' is passed with 'Prelude.Just' as the
--   second (right) argument.
--
-- * If both the target and the overlay 'RoseTrie's have 'Leaf's, both 'Leaf's are passed with
--   'Prelude.Just' to the merging function.
--
-- Also, it is necessary to specify (as the first parameter to this function) the 'RunRoseTrie'
-- type, which indicates 'DepthFirst' or 'BreadthFirst' evaluation.
mergeWithKeyM
  :: forall m p a b c . (Monad m, Ord p)
  => RunRoseTrie
  -> ([p] -> Maybe a -> Maybe b -> m (Maybe c))
  -> (RoseTrie p a -> m (RoseTrie p c))
  -> (RoseTrie p b -> m (RoseTrie p c))
  -> RoseTrie p a -> RoseTrie p b -> m (RoseTrie p c)
mergeWithKeyM control = loop [] where
  loop px merge left right (RoseTrie (leftLeaf, leftBranches)) (RoseTrie (rightLeaf, rightBranches)) = do
    let leaf = merge px leftLeaf rightLeaf
    let map  = liftM (M.fromList . concat) $
           mapM (\ (p, leftIfPaired) -> do
                  tree <- uncurry (loop (px++[p]) merge left right) ||| id $ leftIfPaired
                  return $ if Data.Tree.RoseTrie.null tree then [] else [(p, tree)]
                )
                ( let wrap f = fmap (Right . f) in M.assocs $ 
                    M.mergeWithKey (\ _ a b -> Just $ Left (a, b))
                      (wrap left) (wrap right) leftBranches rightBranches
                )
    if control==BreadthFirst
    then ap (ap (return $        curry RoseTrie) leaf) map
    else ap (ap (return $ flip $ curry RoseTrie) map) leaf

----------------------------------------------------------------------------------------------------
-- $MapLikeFunctions
-- In this section I have made my best effor to create API functions as similar as possible to that
-- of the "Data.Map" module.
----------------------------------------------------------------------------------------------------

alter :: Ord p => (Maybe o -> Maybe o) -> [p] -> RoseTrie p o -> RoseTrie p o
alter f p = runIdentity . alterM (return . f) p

alterM :: (Monad m, Ord p) => (Maybe o -> m (Maybe o)) -> [p] -> RoseTrie p o -> m (RoseTrie p o)
alterM f p = Data.Lens.Minimal.alter (path p) f >=> return . snd

-- | Insert a leaf at a given address, updating it with the combining function if it already exist.
insertWith :: Ord p => (o -> o -> o) -> [p] -> o -> RoseTrie p o -> RoseTrie p o
insertWith append p o = Data.Tree.RoseTrie.alter (Just . maybe o (`append` o)) p 

-- | Insert a leaf at a given address.
insert :: Ord p => [p] -> o -> RoseTrie p o -> RoseTrie p o
insert = insertWith (flip const)

-- | Update a leaf at a given address.
update :: Ord p => (o -> Maybe o) -> [p] -> RoseTrie p o -> RoseTrie p o
update = Data.Tree.RoseTrie.alter . maybe Nothing

-- | Delete a leaf or 'Branch' at a given address.
delete :: Ord p => [p] -> RoseTrie p o -> RoseTrie p o
delete = Data.Tree.RoseTrie.alter (const Nothing)

-- | Create a 'RoseTrie' from a list of associationes, the 'Prelude.fst' element containing the
-- branches, the 'Prelude.snd' element containing the leaf value. This is the inverse operation of
-- 'assocs'.
fromListWith :: Ord p => (o -> o -> o) -> [([p], o)] -> RoseTrie p o
fromListWith append = foldr (uncurry $ insertWith append) Data.Tree.RoseTrie.empty

-- | Like 'fromListWith' but called with @('Prelude.flip' 'Prelude.const')@.
fromList :: Ord p => [([p], o)] -> RoseTrie p o
fromList = fromListWith (flip const)

-- | Create a 'RoseTrie' with @()@ nodes. This is useful for times when the structure of the tree is
-- all you need.
blankRoseTrie :: Ord p => [[p]] -> RoseTrie p ()
blankRoseTrie = fromList . fmap (id &&& const ())

-- | Create a 'RoseTrie' containing only a single 'path' to a single element.
singleton :: Ord p => [p] -> a -> RoseTrie p a
singleton p o = newRoseTrie [path p <~ Just o]

-- | This function analogous to the 'Data.Map.lookup' function, which returns a value stored in a
-- leaf, or nothing if there is no leaf at the given path.
lookup :: Ord p => [p] -> RoseTrie p a -> Maybe a
lookup px = (~> (path px))

-- | This function works like 'lookup', but takes a key predicate to match keys of the tree, rather
-- than using @('Prelude.==')@. This means the efficient O(log n) 'Data.Map.Map' 'Data.Map.lookup'
-- function in the "Data.Map" module cannot be used, each key must be inspected one-by-one making
-- this algorithm O(n^2). This also means multiple values may match the given key predicate. Lookups
-- are always performed in 'DepthFirst' order, this helps improve efficiency a little bit, as the
-- matches nearest the beggining of each list of 'Data.Map.assocs' are chosen first, and lazily
-- taking only the first few matches will save us from searching the entire tree.
--
-- Take note of the different types @p@ and @b@. This means the path @p@ you use to search the
-- 'RoseTrie' need not be the same type as the branches @b@ of the 'RoseTrie', and what is returned
-- are the actual branches @b@ that matched the path @p@, not the path @p@ itself.
slowLookup :: Ord b => (p -> b -> Bool) -> [p] -> RoseTrie b a -> [([b], a)]
slowLookup f = loop [] where
  loop branchPath px t = case px of
    []   -> maybe [] (\o -> [(branchPath, o)]) $ t~>leaf
    p:px -> do
      (b, t) <- filter (f p . fst) (M.assocs $ t~>branches)
      loop (branchPath++[b]) px t

-- | This function calls 'slowLookup' and returns only the first result. This can be used to take
-- advantage of Haskell's laziness and save time by halting the search for matching paths as soon as
-- the first match is found.
slowLookup1 :: Ord b => (p -> b -> Bool) -> [p] -> RoseTrie b a -> Maybe ([b], a)
slowLookup1 f p t = case slowLookup f p t of { [] -> Nothing; o:_ -> Just o; }

-- | Get all items and their associated path.
assocs :: RunRoseTrie -> RoseTrie p a -> [([p], a)]
assocs control = loop [] where
  loop px (RoseTrie (o, m)) =
    (if control==BreadthFirst then id else flip) (++)
      (maybe [] (return . (,) px) o)
      (M.assocs m >>= \ (p, o) -> loop (px++[p]) o)

-- | Like 'assocs' but restricts the resulting list of associations to only include elements that
-- lie along a given path. This function walks through the tree with the given path, and collects
-- every 'leaf' along the way. Where there is a leaf, the path is partitioned into the path up to
-- the leaf and the path after the leaf. The list of returned values are these partitioned paths
-- paired with their associated leaves.
partitions :: (Eq p, Ord p) => RunRoseTrie -> [p] -> RoseTrie p a -> [(([p], [p]), a)]
partitions control = partitionsWith control (\a b -> guard (a == b) >> return a)

-- | Like 'partitions', but allows you to use a matching function that other than ('Prelude.==').
-- The matching function should return 'Prelude.Nothing' for non-matching path elements, and a
-- 'Prelude.Just' containing a path element that may have been transformed by the matching function.
partitionsWith
  :: (Eq p, Ord p)
  => RunRoseTrie -> (p -> q -> Maybe r) -> [q] -> RoseTrie p a -> [(([r], [q]), a)]
partitionsWith control match path = runIdentity .
  partitionWithM control (\a b -> return $ match a b) path

-- | Like 'partitionsWith' but uses a monadic matching function.
partitionWithM
  :: (Eq p, Ord p, Monad m)
  => RunRoseTrie -> (p -> q -> m (Maybe r)) -> [q] -> RoseTrie p a -> m [(([r], [q]), a)]
partitionWithM control match = loop [] where
  getleaf path qx = return . maybe [] (return . (,) (path, qx)) . (~> leaf)
  loop path qx tree = case qx of
    []   -> getleaf path [] tree
    q:qx -> liftM2 ((case control of { DepthFirst -> flip; BreadthFirst -> id; }) (++))
      (getleaf path (q:qx) tree)
      ( liftM concat $ forM (M.assocs $ tree~>branches) $ \ (p, tree) ->
          match p q >>= maybe (return []) (\r -> loop (path++[r]) qx tree)
      )

-- | Apply @'Prelude.map' 'Prelude.snd'@ to the result of 'assocs', behaves just like how
-- 'Data.Map.elems' or 'Data.Array.IArray.elems' works.
elems :: RunRoseTrie -> RoseTrie p a -> [a]
elems control = loop where
  append = (case control of{ DepthFirst -> flip; BreadthFirst -> id; }) (++)
  loop (RoseTrie (a, m)) = append (maybe [] return a) $ M.elems m >>= loop
  -- This function is not implemented in terms of 'assocs' to avoid stacking the paths, as the paths
  -- will be ignored.

-- | Counts the number of *nodes*, which includes the number of 'Branch'es and 'Leaf's.
size :: RoseTrie p a -> Word64
size (RoseTrie (o, m)) = maybe 0 (const 1) o + sum (size <$> M.elems m)

leafCount :: RoseTrie p a -> Word64
leafCount = sum . fmap (const 1) . ReduceRoseTrie DepthFirst

-- | Counts the number of branches, not leaves.
branchCount :: RoseTrie p a -> Word64
branchCount (RoseTrie (_, m)) = fromIntegral (M.size m) + sum (branchCount <$> M.elems m)

null :: RoseTrie p a -> Bool
null (RoseTrie (o, m)) = isNothing o && M.null m

----------------------------------------------------------------------------------------------------

-- | Since this function does not merge trees monadically, it is not important whether merging
-- happens in 'DepthFirst' or 'BreadthFirst' order.
mergeWithKey
  :: Ord p
  => ([p] -> Maybe a -> Maybe b -> Maybe c)
  -> (RoseTrie p a -> RoseTrie p c)
  -> (RoseTrie p b -> RoseTrie p c)
  -> RoseTrie p a -> RoseTrie p b -> RoseTrie p c
mergeWithKey a b c d e = runIdentity $
  mergeWithKeyM BreadthFirst (\k o -> return . a k o) (return . b) (return . c) d e

mergeWithM
  :: (Monad m, Ord p)
  => RunRoseTrie
  -> (Maybe a -> Maybe b -> m (Maybe c))
  -> (RoseTrie p a -> m (RoseTrie p c))
  -> (RoseTrie p b -> m (RoseTrie p c))
  -> RoseTrie p a -> RoseTrie p b -> m (RoseTrie p c)
mergeWithM control f = mergeWithKeyM control (const f)

mergeWith
  :: Ord p
  => (Maybe a -> Maybe b -> Maybe c)
  -> (RoseTrie p a -> RoseTrie p c)
  -> (RoseTrie p b -> RoseTrie p c)
  -> RoseTrie p a -> RoseTrie p b -> RoseTrie p c
mergeWith f = mergeWithKey (const f)

----------------------------------------------------------------------------------------------------

unionWithKeyM
  :: (Monad m, Ord p)
  => RunRoseTrie
  -> ([p] -> a -> a -> m a)
  -> RoseTrie p a -> RoseTrie p a -> m (RoseTrie p a)
unionWithKeyM control f =
  mergeWithKeyM control
    (\k a b -> maybe (return Nothing) (>>= (return . Just)) $
        (f <$> pure k <*> a <*> b) <|> fmap return a <|> fmap return b
      ) return return

unionWithKey :: Ord p => ([p] -> a -> a -> a) -> RoseTrie p a -> RoseTrie p a -> RoseTrie p a
unionWithKey f a = runIdentity . unionWithKeyM BreadthFirst (\k a -> return . f k a) a

unionWithM :: (Monad m, Ord p) => RunRoseTrie -> (a -> a -> m a) -> RoseTrie p a -> RoseTrie p a -> m (RoseTrie p a)
unionWithM control f = unionWithKeyM control (const f)

unionWith :: Ord p => (a -> a -> a) -> RoseTrie p a -> RoseTrie p a -> RoseTrie p a
unionWith f a = runIdentity . unionWithM BreadthFirst (\a -> return . f a) a

union :: Ord p => RoseTrie p a -> RoseTrie p a -> RoseTrie p a
union = unionWith const

unionsWith :: Ord p => (a -> a -> a) -> [RoseTrie p a] -> RoseTrie p a
unionsWith overlap = foldl (unionWith overlap) Data.Tree.RoseTrie.empty

unions :: Ord p => [RoseTrie p a] -> RoseTrie p a
unions = unionsWith (flip const)

----------------------------------------------------------------------------------------------------

intersectionWithKeyM
  :: (Monad m, Ord p)
  => RunRoseTrie
  -> ([p] -> a -> b -> m c)
  -> RoseTrie p a -> RoseTrie p b -> m (RoseTrie p c)
intersectionWithKeyM control f =
  mergeWithKeyM control
    (\k a b -> maybe (return Nothing) (>>= (return . Just)) $ f <$> pure k <*> a <*> b)
      (return . const Data.Tree.RoseTrie.empty) (return . const Data.Tree.RoseTrie.empty)

intersectionWithKey :: Ord p => ([p] -> a -> b -> c) -> RoseTrie p a -> RoseTrie p b -> RoseTrie p c
intersectionWithKey f a = runIdentity . intersectionWithKeyM BreadthFirst (\k a -> return . f k a) a

intersectionWithM :: (Monad m, Ord p) => RunRoseTrie -> (a -> b -> m c) -> RoseTrie p a -> RoseTrie p b -> m (RoseTrie p c)
intersectionWithM control f = intersectionWithKeyM control (const f)

intersectionWith :: Ord p => (a -> b -> c) -> RoseTrie p a -> RoseTrie p b -> RoseTrie p c
intersectionWith f a = runIdentity . intersectionWithM BreadthFirst (\a -> return . f a ) a

intersection :: Ord p => RoseTrie p a -> RoseTrie p b -> RoseTrie p a
intersection = intersectionWith const

intersectionsWith :: Ord p => (a -> a -> a) -> [RoseTrie p a] -> RoseTrie p a
intersectionsWith overlap = foldl (intersectionWith overlap) Data.Tree.RoseTrie.empty

intersections :: Ord p => [RoseTrie p a] -> RoseTrie p a
intersections = intersectionsWith (flip const)

----------------------------------------------------------------------------------------------------

differenceWithKeyM
  :: (Monad m, Ord p)
  => RunRoseTrie
  -> ([p] -> a -> b -> m (Maybe a))
  -> RoseTrie p a -> RoseTrie p b -> m (RoseTrie p a)
differenceWithKeyM control f =
  mergeWithKeyM control
    (\k a b -> fromMaybe (return Nothing) $ (f <$> pure k <*> a <*> b) <|> fmap (return . Just) a)
      return (return . const Data.Tree.RoseTrie.empty)

differenceWithKey :: Ord p => ([p] -> a -> b -> Maybe a) -> RoseTrie p a -> RoseTrie p b -> RoseTrie p a
differenceWithKey f a = runIdentity . differenceWithKeyM BreadthFirst (\k a -> return . f k a) a

differenceWithM :: (Monad m, Ord p) => RunRoseTrie -> (a -> b -> m (Maybe a)) -> RoseTrie p a -> RoseTrie p b -> m (RoseTrie p a)
differenceWithM control f = differenceWithKeyM control (const f)

differenceWith :: Ord p => (a -> b -> Maybe a) -> RoseTrie p a -> RoseTrie p b -> RoseTrie p a
differenceWith f a = runIdentity . differenceWithM BreadthFirst (\a -> return . f a) a

difference :: Ord p => RoseTrie p a -> RoseTrie p b -> RoseTrie p a
difference = differenceWith (\ _ _ -> Nothing)

differencesWith :: Ord p => (a -> a -> Maybe a) -> [RoseTrie p a] -> RoseTrie p a
differencesWith overlap = foldl (differenceWith overlap) Data.Tree.RoseTrie.empty

differences :: Ord p => [RoseTrie p a] -> RoseTrie p a
differences = differencesWith (\ _ _ -> Nothing)

----------------------------------------------------------------------------------------------------

-- | This function computes the cartesian of two trees. For example, if the 'assocs' of two trees
-- are:
--
-- @
-- -- tree X              tree Y
-- [( [a, b, c], t ),  [( [b, c], w ),
--  ( [a, b   ], u ),   ( [a   ], x )]
--  ( [b      ], v )]
-- @
--
-- Then the 'product' of these two trees X and Y is the evaluation of 'fromList' on:
--
-- @
-- [( [a, b, c] ++ [b, c], t<>w ),
--  ( [a, b, c] ++ [a   ], t<>x ),
--  ( [a, b,  ] ++ [b, c], u<>w ),
--  ( [a, b,  ] ++ [a,  ], u<>x ),
--  ( [b,     ] ++ [b, c], v<>w ),
--  ( [b,     ] ++ [a   ], v<>x )]
-- @
productWith :: Ord p => (a -> b -> c) -> RoseTrie p a -> RoseTrie p b -> RoseTrie p c
productWith append a b = fromList $ do
  (pA, oA) <- assocs BreadthFirst a
  (pB, oB) <- assocs BreadthFirst b
  [(pA++pB, append oA oB)]

-- | Like 'productWith' but uses 'Data.Monoid.mappend' as the function that computes the product of
-- each element.
product :: (Ord p, Monoid a) => RoseTrie p a -> RoseTrie p a -> RoseTrie p a
product = productWith mappend

----------------------------------------------------------------------------------------------------

-- | If you have read the chapter about zippers in the book "Learn You a Haskell for Great Good",
-- you might appreciate that a zipper is provided for 'RoseTrie' in this module, and a number of
-- useful "Control.Monad.State"ful APIs are also provided, namely 'goto' and 'back'.
-- 
-- Although it should be noted usually, 'Dao.Lens.Lens'es, 'Data.Foldable.fold's,
-- 'Data.Traversable.traversal's, and 'mergeWithKeyM' are all you will need.
data ZipRoseTrie p o = ZipRoseTrie (RoseTrie p o) [(p, RoseTrie p o)] deriving (Eq, Ord, Typeable)

zipperSubRoseTrie :: Monad m => Lens m (ZipRoseTrie p o) (RoseTrie p o)
zipperSubRoseTrie = newLens (\ (ZipRoseTrie t _) -> t) (\t (ZipRoseTrie _ h) -> ZipRoseTrie t h)

zipperHistory :: Monad m => Lens m (ZipRoseTrie p o) [(p, RoseTrie p o)]
zipperHistory = newLens (\ (ZipRoseTrie _ h) -> h) (\h (ZipRoseTrie t _) -> ZipRoseTrie t h)

-- | A monadic function type that keeps the 'ZipRoseTrie' in a 'Control.Monad.State.StateT' for you, and
-- instantiates 'Control.Monad.State.MonadState' such that 'Control.Monad.State.get' and
-- 'Control.Monad.State.put' operate on leaves of the 'RoseTrie'. Use 'goto', 'back', and 'home' to
-- navigate the 'RoseTrie'.
newtype UpdateRoseTrieT p o m a = UpdateRoseTrieT (StateT (ZipRoseTrie p o) m a)
type UpdateRoseTrie  p o   a = UpdateRoseTrieT p o Identity a

instance Functor m => Functor (UpdateRoseTrieT p o m) where
  fmap f (UpdateRoseTrieT o) = UpdateRoseTrieT $ fmap f o

instance (Functor m, Applicative m, Monad m) => Applicative (UpdateRoseTrieT p o m) where
  pure = UpdateRoseTrieT . pure
  (UpdateRoseTrieT f) <*> (UpdateRoseTrieT o) = UpdateRoseTrieT (f <*> o)

instance (Functor m, Applicative m, Monad m) => Monad (UpdateRoseTrieT p o m) where
  return = pure
  (UpdateRoseTrieT o) >>= f = UpdateRoseTrieT $ o >>= (\ (UpdateRoseTrieT o) -> o) . f

instance (Ord p, Functor m, Applicative m, Monad m) => MonadState (Maybe o) (UpdateRoseTrieT p o m) where
  state f = UpdateRoseTrieT $ StateT $ \st -> do
    (a, l) <- return $ f $ st~>zipperSubRoseTrie~>leaf
    return (a, with st [zipperSubRoseTrie >>> leaf <~ l])

instance MonadTrans (UpdateRoseTrieT p o) where { lift = UpdateRoseTrieT . lift; }

-- | Run the 'UpdateRoseTrieT' function, returning the modified 'RoseTrie' and the last result returned by
-- the 'UpdateRoseTrieT' function.
runUpdateRoseTrieT :: (Functor m, Applicative m, Monad m, Ord p) => UpdateRoseTrieT p o m a -> RoseTrie p o -> m (a, RoseTrie p o)
runUpdateRoseTrieT f tree = do
  (a, z) <- runStateT ((\ (UpdateRoseTrieT f) -> f) $ f <* home) $ ZipRoseTrie tree []
  return (a, z~>zipperSubRoseTrie)

-- | Analogous to 'Control.Monad.State.execStateT', does the same thing as 'runUpdateRoseTrieT' but
-- disgards the final return value of the 'UpdateRoseTrieT' function.
execUpdateRoseTrieT :: (Functor m, Applicative m, Monad m, Ord p) => UpdateRoseTrieT p o m a -> RoseTrie p o -> m (RoseTrie p o)
execUpdateRoseTrieT f = fmap snd . runUpdateRoseTrieT f

-- | Analogous to 'Control.Monad.State.execStateT', does the same thing as 'runUpdateRoseTrieT' but
-- disgards the updated 'RoseTrie' and only keeps the last return value of the 'UpdateRoseTrieT' function.
evalUpdateRoseTrieT :: (Functor m, Applicative m, Monad m, Ord p) => UpdateRoseTrieT p o m a -> RoseTrie p o -> m a
evalUpdateRoseTrieT f = runUpdateRoseTrieT f >=> return . fst

-- | Go to the node with the given path. If the path does not exist, it is created.
goto :: (Functor m, Applicative m, Monad m, Ord p) => [p] -> UpdateRoseTrieT p o m ()
goto px = case px of
  []       -> return ()
  (p:px) -> do
    UpdateRoseTrieT $ do
      t <- gets $ fromMaybe Data.Tree.RoseTrie.empty . M.lookup p . (~> (branches . zipperSubRoseTrie))
      modify (\st -> with st [zipperSubRoseTrie <~ t, zipperHistory $= ((p, st~>zipperSubRoseTrie) :)])
    goto px

-- | Go up one level in the tree, storing the current sub-tree into the upper tree, unless the
-- current tree is 'Void', in which case it is deleted from the upper tree. Returns 'Prelude.False'
-- if we are already at the root of the 'RoseTrie' and could not go back.
back :: (Functor m, Applicative m, Monad m, Ord p) => UpdateRoseTrieT p o m Bool
back = UpdateRoseTrieT $ state $ \st -> case st~>zipperHistory of
  []                    -> (False, st)
  (p, RoseTrie (t, m)):hist -> (,) True $ let u = st~>zipperSubRoseTrie in with st
    [ zipperSubRoseTrie <~ RoseTrie (t, (if Data.Tree.RoseTrie.null u then id else M.insert p u) m)
    , zipperHistory <~ hist
    ]

-- | Returns 'Prelude.True' if we are at the top level of the tree.
atTop :: (Functor m, Applicative m, Monad m) => UpdateRoseTrieT p o m Bool
atTop = Prelude.null <$> UpdateRoseTrieT (gets (~> zipperHistory))

-- | Go back to the top level of the tree.
home :: (Functor m, Applicative m, Monad m, Ord p) => UpdateRoseTrieT p o m ()
home = atTop >>= flip unless (back >> home)

-- | Return the current path.
getPath :: (Functor m, Applicative m, Monad m, Ord p) => UpdateRoseTrieT p o m [p]
getPath = reverse . fmap fst <$> UpdateRoseTrieT (gets (~> zipperHistory))

----------------------------------------------------------------------------------------------------

-- | This data type lets you store a "diff", that is a structure tracking the differences, between
-- two 'RoseTrie's. This is essentially the result of a 'mergeWithKeyM' operation tracking all of the
-- changes that would happen in a data structure without actually applying the changes. Traversing
-- over the 'RoseTrie' of 'RoseTrieDiff's with 'Data.Traversable.traverse' to actually convert the
-- 'RoseTrieDiff's would then apply the changes.
data RoseTrieDiff a b
  = LeftOnly  a -- something exists in the "left" branches but not in the "right" branches.
  | RightOnly b -- something exists in the "right" branches but not in the "left" branches.
  | RoseTrieDiff  a b -- something exists in the "left" and "right" branches but they are not equal
  deriving (Eq, Typeable)

-- | Produce a difference report of two trees with the given comparison predicate. If the predicate
-- returns 'Prelude.True', the node does not appear in the resultant 'RoseTrie'. If there is a
-- difference, the difference is recored into a node in the resultant 'RoseTrie'.
treeDiffWithM
  :: forall m p a b . (Monad m, Ord p)
  => RunRoseTrie
  -> ([p] -> a -> b -> m Bool)
  -> RoseTrie p a -> RoseTrie p b -> m (RoseTrie p (RoseTrieDiff a b))
treeDiffWithM control compare =
  mergeWithKeyM control merge (return . fmap LeftOnly) (return . fmap RightOnly) where
    merge p a b = fromMaybe (return Nothing) $ msum
      [ a >>= \a -> b >>= \b -> return $
          compare p a b >>= \same -> return $ if same then Nothing else return $ RoseTrieDiff a b
      , a >>= Just . return . Just . LeftOnly
      , b >>= Just . return . Just . RightOnly
      ]

treeDiffWith :: Ord p => ([p] -> a -> b -> Bool) -> RoseTrie p a -> RoseTrie p b -> RoseTrie p (RoseTrieDiff a b)
treeDiffWith f a = runIdentity . treeDiffWithM BreadthFirst (\p a -> return . f p a) a

-- | Call 'treeDiffWith' using 'Prelude.(==)' as the comparison predicate.
treeDiffM :: (Monad m, Eq a, Ord p) => RunRoseTrie -> RoseTrie p a -> RoseTrie p a -> m (RoseTrie p (RoseTrieDiff a a))
treeDiffM control = treeDiffWithM control (\ _ a -> return . (a ==))

-- | Call 'treeDiffWith' using 'Prelude.(==)' as the comparison predicate.
treeDiff :: (Eq a, Ord p) => RoseTrie p a -> RoseTrie p a -> RoseTrie p (RoseTrieDiff a a)
treeDiff a = runIdentity . treeDiffM BreadthFirst a

