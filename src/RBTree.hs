module RBTree where

data RBTree a = Nil | Node Color (RBTree a) a (RBTree a)
            deriving (Show, Eq)

data Color = Red | Black
            deriving (Show, Eq)

newRBTree :: RBTree a
newRBTree = Nil

blacken :: RBTree a -> RBTree a
blacken (Node c l v r) = Node Black l v r
blacken t = t

-- if meet a value already in the RBTree, discard it.
-- See line "otherwise = t"

insertRBTree :: (Ord a) => RBTree a -> a -> RBTree a
insertRBTree t x= blacken $ insert' t x
    where   insert' Nil x = Node Red Nil x Nil 
            insert' t@(Node c l v r) x      | v > x = blanceRBTree $ Node c (insert' l x) v r
                                            | v < x = blanceRBTree $ Node c l v (insert' r x)
                                            | otherwise = t

-- the sequences are LL, LR, RL, RR
blanceRBTree :: (Ord a) => RBTree a -> RBTree a
blanceRBTree (Node _ (Node Red (Node Red a x b) y c) z d) = Node Red (Node Black a x b) y (Node Black c z d)
blanceRBTree (Node _ (Node Red a y (Node Red b x c)) z d) = Node Red (Node Black a y b) x (Node Black c z d)
blanceRBTree (Node _ a z (Node Red (Node Red b x c) y d)) = Node Red (Node Black a z b) x (Node Black c y d)
blanceRBTree (Node _ a z (Node Red b y (Node Red c x d))) = Node Red (Node Black a z b) y (Node Black c x d)
blanceRBTree t = t

deleteRBTree :: (Ord a) => RBTree a -> a -> RBTree a
deleteRBTree t x = fst $ delete' t x
    where   delete' t@(Node c l v r) x  | v > x = (Node c (fst $ delete' l x) v r, True)
                                        | v < x = (Node c l v (fst $ delete' r x), True)
                                        | otherwise = (fst $ deleteThis t, True)
            deleteThis (Node c Nil _ Nil)   = (Nil, c == Red)
            deleteThis (Node c l _ Nil)     = (l, True)
            deleteThis (Node c Nil _ r)     = (r, True)
            deleteThis (Node c l _ r)       = let min = findMin r in (Node c l min (fst $ delete' r min), True)
            findMin (Node _ Nil x _ )       = x
            findMin (Node _ l _ _)          = findMin l





