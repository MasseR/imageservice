module Data.Trie where

data Trie a = Root [Trie a] | Leaf Char (Maybe a) [Trie a]
            deriving (Show)

-- XXX: Implement this at a later date with better time (especially if memory is starting to be a problem)

-- insert :: String -> a -> Trie a -> Trie a
-- insert [] _ t = t
-- insert [x] v (Root _) = Leaf x (Just v) []
-- insert [x] v (Leaf _ _ cs) = Leaf x (Just v) cs
-- insert (x:xs) v (Root cs) = Root (alter 
--
-- alter :: (a -> Bool) -> (a -> a) -> a -> [a] -> [a]
-- alter _ _ v [] = [v]
-- alter f alt v (x:xs) = if f x then alt x : xs else x : alter f alt v xs
