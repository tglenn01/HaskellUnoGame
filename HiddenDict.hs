-- HiddenDict module adapted from TreeDict.hs, (c) 2024 David Poole
-- Original source retrieved from https://www.cs.ubc.ca/~poole/cs312/2024/haskell/TreeDict.hs

module HiddenDict (
    HiddenDict,
    emptyDict,
    get,
    insert,
    update,
    tolist,
    size,
) where

data BSTree k v = Empty
    | Node k v (BSTree k v) (BSTree k v)
instance Show (BSTree k v) where
    show d = "[Dict: " ++ show (size d) ++ "]"

type HiddenDict = BSTree

emptyDict :: HiddenDict k v
emptyDict = Empty

get :: (Ord k) => k -> HiddenDict k v -> Maybe v
get key (Node kt val l r)
    | key == kt    = Just val
    | key < kt    = get key l
    | otherwise = get key r
get _ Empty = Nothing

insert :: (Ord k) => k -> v -> HiddenDict k v -> HiddenDict k v
insert key val Empty = Node key val Empty Empty
insert key val (Node kt vt l r)
    | key == kt = Node kt val l r   -- replace value
    | key < kt  = Node kt vt (insert key val l) r
    | otherwise = Node kt vt l (insert key val r)

update :: (Ord k) => k -> (Maybe v -> v) -> HiddenDict k v -> HiddenDict k v
update key fun Empty = Node key (fun Nothing) Empty Empty
update key fun (Node kt vt l r)
     | key == kt = Node kt (fun (Just vt)) l r   -- replace value
     | key < kt  = Node kt vt (update key fun l) r
     | otherwise = Node kt vt l (update key fun r)

tolist :: HiddenDict a b -> [Char]
tolist Empty = []
tolist (Node key val l r) = tolist l ++ ['?'] ++ tolist r

size :: HiddenDict a b -> Int
size d = length (tolist d)
