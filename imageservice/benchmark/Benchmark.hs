{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE TypeOperators  #-}
module Main where

import Data.Monoid (Endo(..))

import           GHC.Generics

import           Data.Foldable            (foldl')

import           Criterion
import           Criterion.Main           (defaultMain)

import           Data.BKTree              (BKTree)
import qualified Data.BKTree              as BK

import           System.Random

import           Test.QuickCheck.Deriving

import           Control.DeepSeq          (NFData)

data Point = Point !Int !Int
           deriving stock (Show, Generic, Ord, Eq)
           deriving anyclass (NFData)
           deriving BK.Metric via ((Int, Int) `Isomorphic` Point)

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA 0 _ = pure []
replicateA n f = (:) <$> f <*> replicateA (n - 1) f

setupEnv :: IO ([Point], [Point])
setupEnv = (,) <$> replicateA 100 randomPoint <*> replicateA 10000 randomPoint
  where
    randomPoint = Point <$> randomIO <*> randomIO

benchFoldrInsert :: [Point] -> BKTree Point
benchFoldrInsert = foldr BK.insert mempty

benchFoldMapInsert :: [Point] -> BKTree Point
benchFoldMapInsert = foldMap BK.singleton

benchEndoInsert :: [Point] -> BKTree Point
benchEndoInsert ps = appEndo (foldMap (\p -> Endo (<> BK.singleton p)) ps) mempty

benchFoldlInsert :: [Point] -> BKTree Point
benchFoldlInsert = foldl' (flip BK.insert) mempty

bkResults :: BKTree a -> Int
bkResults = length

main :: IO ()
main = defaultMain
  [ env setupEnv $ \(~(small,large)) -> bgroup "main"
    [
      bgroup "small"
      [ bench "foldr insert" $ whnf (bkResults . benchFoldrInsert) small
      , bench "foldl' insert" $ whnf (bkResults . benchFoldlInsert) small
      , bench "foldMap insert" $ whnf (bkResults . benchFoldMapInsert) small
      , bench "foldMap Endo insert" $ whnf (bkResults . benchEndoInsert) small
      ]
    , bgroup "large"
      [ bench "foldr insert" $ whnf (bkResults . benchFoldrInsert) large
      , bench "foldl' insert" $ whnf (bkResults . benchFoldlInsert) large
      -- , bench "foldMap insert" $ whnf (bkResults . benchFoldMapInsert) large
      ]
    ]
  ]
