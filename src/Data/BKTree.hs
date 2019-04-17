{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}
module Data.BKTree where

import           Data.Foldable            (foldMap)
import           Data.Functor.Foldable
import           Data.Functor.Foldable.TH
import           Data.List                (foldl')
import           Data.Monoid              (Endo (..))
import           Data.SafeCopy
import           GHC.Generics             (Generic)

-- Point for testing purposes
data Point = Point Int Int deriving Show

instance Metric Point where
  distance (Point p1 p2) (Point q1 q2) = abs (p1 - q1) + abs (p2 - q2)

class Metric a where
  distance :: a -> a -> Int

data Tuple a = Tuple !Int a deriving (Show, Functor, Foldable, Traversable)

data BKTree a = Empty
              | Node !a [Tuple (BKTree a)] deriving (Show, Generic, Functor, Traversable, Foldable)

makeBaseFunctor ''BKTree
deriveSafeCopy 0 'base ''Tuple
deriveSafeCopy 0 'base ''BKTree

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
  Empty -> Node a []
  Node b children ->
    let newDistance = distance a b
    in Node b (addChild newDistance children)
  where
    addChild d = \case
      [] -> [Tuple d  (insert a Empty)]
      Tuple d' child:children | d == d' -> Tuple d' (insert a child) : children
                          | otherwise -> Tuple d' child : addChild d children


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
            filteredChildren = concat [xs | Tuple d xs <- children, d <= upper, d >= lower]
        in if thisDistance <= n then x : filteredChildren else filteredChildren
