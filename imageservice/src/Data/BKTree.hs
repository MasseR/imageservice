{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Data.BKTree where

import qualified Data.Foldable            as F
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                (foldl')
import           Data.Monoid              (Endo (..))
import           GHC.Generics             (Generic)

import qualified Data.Set                 as S

-- For testing
import           Test.QuickCheck
import           Test.QuickCheck.Deriving

data Tree a = Tree !(Tree a) !a (Tree a) | EmptyLeaf
            deriving (Show, Functor, Traversable, Foldable, Generic, Eq)



class Metric a where
  distance :: a -> a -> Int

data Tuple a = Tuple !Int !a deriving (Show, Functor, Foldable, Traversable, Generic, Eq)

data BKTree a = Empty
              | Node !a (Tree (Tuple (BKTree a))) -- [Tuple (BKTree a)]
              deriving (Show, Generic, Functor, Traversable, Foldable)

makeBaseFunctor ''BKTree

instance (Metric a, Arbitrary a) => Arbitrary (BKTree a) where
  arbitrary = fromList <$> arbitrary
  shrink = shrinkMap fromList toList


empty :: BKTree a
empty = Empty

singleton :: Metric a => a -> BKTree a
singleton a = insert a empty

fromList :: Metric a => [a] -> BKTree a
fromList = foldl' (flip insert) empty

toList :: BKTree a -> [a]
toList tree = appEndo (foldMap (\x -> Endo ([x] ++)) tree) []

insert :: Metric a => a -> BKTree a -> BKTree a
insert a = \case
  Empty -> Node a EmptyLeaf
  Node b children ->
    let newDistance = distance a b
        newChildren = addChild newDistance children
    in newChildren `seq` Node b newChildren
  where
    addChild d = \case
      EmptyLeaf -> let x = Tuple d (insert a Empty) in x `seq` Tree EmptyLeaf x EmptyLeaf
      Tree l trg@(Tuple d' child) r
        | d' == d -> let x = Tuple d' (insert a child) in x `seq` Tree l x r
        | d' < d -> let l' = addChild d l in l' `seq` Tree l' trg r
        | otherwise -> let r' = addChild d r in r' `seq` Tree l trg r'
{-# INLINE insert #-}


search :: forall a. Metric a => Int -> a -> BKTree a -> [a]
search n a = cata alg
  where
    alg :: BKTreeF a [a] -> [a]
    alg = \case
      EmptyF -> []
      NodeF x children ->
        let thisDistance = distance a x
            upper = thisDistance + n
            lower = thisDistance - n
            filteredChildren = concat [xs | Tuple d xs <- F.toList children, d <= upper, d >= lower]
        in if thisDistance <= n then x : filteredChildren else filteredChildren

-- When comparing associativity, the internal representation might not be equal
instance Metric a => Semigroup (BKTree a) where
  a <> b = foldl' (flip insert) a b

instance Metric a => Monoid (BKTree a) where
  mempty = Empty

-- Visibly equal, the internal representation might differ
instance (Ord a, Eq a) => Eq (BKTree a) where
  a == b = S.fromList (toList a) == S.fromList (toList b)
