

data Node a = Value a (Node a) (Node a) | Empty



insert :: Ord a => Node a -> a -> Node a
insert Empty v = Value v Empty Empty
insert (Value x left right) v = Value x left' right'
    where
        left' = if v > x then left else insert left v
        right' = if v < x then right else insert right v

instance Show a => Show (Node a) where
    show (Value a Empty right) = show a ++ show right
    show (Value a left Empty) = show a ++ show left
    show (Value a left right) = concat [show a, "\n", show left, " ", show right]
    show Empty = "null"

