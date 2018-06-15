module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = BST { val   :: Maybe a,
                   left  :: Maybe (BST a),
                   right :: Maybe (BST a)
                 } deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft = left

bstRight :: BST a -> Maybe (BST a)
bstRight = right

bstValue :: BST a -> Maybe a
bstValue = val

empty :: BST a
empty = BST { val=Nothing, left=Nothing, right=Nothing }

fromList :: Ord a => [a] -> BST a
fromList = foldl (flip insert) empty

insert :: Ord a => a -> BST a -> BST a
insert x tree = case val tree of
  Nothing -> singleton x
  Just a  -> BST { val=val tree, left=leftTree, right=rightTree } 
                  where
                    (leftTree,rightTree) = if x > a
                                             then (left tree, insert' $ right tree)
                                             else (insert' $ left tree, right tree)
                    insert' Nothing = Just $ singleton x
                    insert' (Just subTree) = Just $ insert x subTree

singleton :: a -> BST a
singleton x = BST { val=Just x, left=Nothing, right=Nothing }

toList :: BST a -> [a]
toList tree = toList' (left tree) ++ rootList ++ toList' (right tree)
  where
    toList' Nothing  = []
    toList' (Just t) = toList t
    rootList         = case val tree of
      Nothing  -> []
      Just val -> [val]
    
