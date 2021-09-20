{-# LANGUAGE RankNTypes, GADTs, KindSignatures #-}
module RedBlack
where
  
import Prelude hiding (lookup)

data Zero
data Succ n

data Red
data Black

data RB :: * -> * -> * -> * -> * where
  Empty :: forall color  key value . RB color Zero key value
  Red   :: forall n key value .
           RB Black n key value -> key -> value -> RB Black n key value ->
           RB Red n key value
  Black :: forall n color1 color2 key value .
           RB color1 n key value -> key -> value -> RB color1 n key value ->
           RB Black (Succ n) key value

lookup :: Ord key => key -> RB color n key value -> Maybe value
lookup k Empty = Nothing
lookup k (Red left k' v right) =
  case compare k k' of
    EQ -> Just v
    LT -> lookup k left
    GT -> lookup k right
lookup k (Black left k' v right) =
  case compare k k' of
    EQ -> Just v
    LT -> lookup k left
    GT -> lookup k right

-- type family higher 
