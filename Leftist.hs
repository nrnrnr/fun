{-# OPTIONS -Wall -Werror -fno-warn-name-shadowing #-}
{-# LANGUAGE PatternGuards #-}

module Leftist 
{-
  ( singleton
  , isEmpty
  , merge
  , insert
  , deletemin
  , Heap
  , heapsort, elements
  )
-}
where
  
import Control.Monad
import Data.List hiding (insert)
import Test.QuickCheck hiding (elements)

data Heap a = Heap { val :: a
                   , left :: Heap a
                   , right :: Heap a
                   , s :: Int   -- shortest path to a leaf
                   }
            | Empty
  deriving Show

slow_svalue :: Heap a -> Int
slow_svalue Empty = 0
slow_svalue (Heap { left = left, right = right }) =
  succ (min (slow_svalue left) (slow_svalue right))

-- | The fast S-value function
svalue :: Heap a -> Int
svalue Empty = 0
svalue h = s h 




isEmpty :: Heap a -> Bool
isEmpty Empty = True
isEmpty (Heap {}) = False

singleton :: a -> Heap a
singleton a = Heap { val = a, left = Empty, right = Empty, s = 1 }

insert :: Ord a => a -> Heap a -> Heap a
insert a heap = merge (singleton a) heap

deletemin :: Ord a => Heap a -> Maybe (a, Heap a)
deletemin Empty = Nothing
deletemin (h @ Heap { }) = Just (val h, merge (left h) (right h))

-- | Three heaps; we know which one has the smallest S-value
data Heaps3 a = H3 { smallest :: Heap a
                   , larger1  :: Heap a
                   , larger2  :: Heap a
                   }

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty h = h
merge h Empty = h
merge h1 h2 
  | val h1 > val h2 = merge h2 h1
  | otherwise       = -- h1 has the smaller root
      Heap { val = val h1
           , left = merge (larger1 three) (larger2 three)
           , right = smallest three
           , s = succ (svalue (smallest three))
           }
        where three = heaps3 (left h1) (right h1) h2
              
heaps3 :: Heap a -> Heap a -> Heap a -> Heaps3 a
heaps3 h1 h2 h3 =
  if svalue h1 < svalue h2 then
    if svalue h1 < svalue h3 then
      H3 h1 h2 h3
    else
      H3 h3 h1 h2
  else
    if svalue h2 < svalue h3 then
      H3 h2 h1 h3
    else
      H3 h3 h1 h2
      
-----------------------------------------------------------------

heapsort :: Ord a => [a] -> [a]
heapsort as = elements (foldr insert Empty as)

elements :: Ord a => Heap a -> [a]
elements h
  | Just (a, h') <- deletemin h = a : elements h'
  | otherwise = []

---------------------------------

sortWorks :: [Int] -> Bool
sortWorks ns = heapsort ns == sort ns


instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = liftM (foldr insert Empty) arbitrary
  coarbitrary = error "never"

------------------------------

cached_s_good :: Heap Int -> Bool
cached_s_good Empty = True
cached_s_good h = 
  slow_svalue h == s h && cached_s_good (left h) && cached_s_good (right h)

------------------------------

leftist :: Heap Int -> Bool
leftist Empty = True
leftist h = svalue (left h) >= svalue (right h)


-------------------------------------------------------

degenerate :: Heap a -> Bool
degenerate Empty = True
degenerate Heap { right = Empty } = True
degenerate Heap { right = Heap { left = Empty, right = Empty } } = True
degenerate _ = False
