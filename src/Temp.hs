module Temp where

data RBTree a = Nil | Node Color a (RBTree a) (RBTree a) deriving(Show)
data Color = Red | Black deriving (Show, Eq)

isBlack (Node Red _ _ _) = False
isBlack _ = True

blacken Nil = Nil
blacken (Node _ value left right) = Node Black value left right


balL color y (left, True) right = (Node color y left right, True)
balL color y (left, False) right = balL' color y left right

balR color y left (right, True) = (Node color y left right, True)
balR color y left (right, False) = balR' color y left right

balL' color1 p n (Node color2 s sl sr)
    | color2 == Red = balL Black s (balL' Red p n sl) sr
    | isBlack sl && isBlack sr = (Node Black p n (Node Red s sl sr), color1 == Red)
    | not (isBlack sr) = (Node color1 s (Node Black p n sl) (blacken sr), True)
    | otherwise = let (Node Red x sll slr) = sl in balL' color1 p n (Node Black x sll (Node Red s slr sr))

balR' color1 p (Node color2 s sl sr) n
    | color2 == Red = balR Black s sl (balR' Red p sr n)
    | isBlack sl && isBlack sr = (Node Black p (Node Red s sl sr) n, color1 == Red)
    | not (isBlack sl) = (Node color1 s (blacken sl) (Node Black p sr n), True)
    | otherwise = let (Node Red x srl srr) = sr in balR' color1 p (Node Black x (Node Red s sl srl) srr) n

delete x t = fst $ delete' x t
  where delete' x Nil = (Nil, True)
        delete' x root@(Node color y left right)
            | x < y = balL color y (delete' x left) right
            | x > y = balR color y left (delete' x right)
            | otherwise = deleteRoot root

        -- it is hard for `color == Black`
        deleteRoot (Node color _ Nil Nil) = (Nil, color == Red)


        -- when it has only one child, child must be Red
        -- if not, the Black Height is same for left and right
        deleteRoot (Node _ _ left Nil) = (blacken left, True)
        deleteRoot (Node _ _ Nil right) = (blacken right, True)

        -- when it has both left and right child
        -- find the min value in right child to replace the node to delete,
        -- and the delete then replaced `to delete` node
        deleteRoot (Node color _ left right) =  let m = findMin right 
                                                in balR color m left (delete' m right)

        -- aux function to find min value
        findMin (Node _ x Nil _) = x
        findMin (Node _ _ left _) = findMin left