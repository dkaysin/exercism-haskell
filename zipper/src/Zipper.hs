module Zipper
 ( BinTree(BT)
 , fromTree
 , left
 , right
 , setLeft
 , setRight
 , setValue
 , toTree
 , up
 , value
 ) where

data BinTree a = BT { btValue :: a
                    , btLeft  :: Maybe (BinTree a)
                    , btRight :: Maybe (BinTree a)
                    } | Null deriving (Eq, Show)

data Direction = L | R deriving (Eq, Show)
data Zipper a = Zipper { parent :: BinTree a
                       , focus  :: BinTree a
                       , dir    :: [Direction]
                       } deriving (Eq, Show)

fromTree :: BinTree a -> Zipper a
fromTree tree = Zipper{ parent = Null, focus = tree, dir = [] }

toTree :: Zipper a -> BinTree a
toTree = focus . toTop

toTop :: Zipper a -> Zipper a
toTop zp = case newZpM of
  (Just newZp) -> toTop newZp
  Nothing      -> zp
  where newZpM = up zp

value :: Zipper a -> a
value = btValue . focus

left :: Zipper a -> Maybe (Zipper a)
left Zipper{ parent = p, focus = f, dir = xd } = do
  l <- btLeft f
  return $ Zipper BT{ btValue = btValue f, btLeft = Just p, btRight = btRight f } l (L:xd)

right :: Zipper a -> Maybe (Zipper a)
right Zipper{ parent = p, focus = f, dir = xd } = do
  r <- btRight f
  return $ Zipper BT{ btValue = btValue f, btLeft = btLeft f, btRight = Just p } r (R:xd)

up :: Zipper a -> Maybe (Zipper a)
up Zipper{ dir = [] }  = Nothing
up Zipper{ parent = p, focus = f, dir = (R:xd) } = do
  r <- btRight p
  return $ Zipper r BT{ btValue = btValue p, btLeft = btLeft p, btRight = Just f } xd
up Zipper{ parent = p, focus = f, dir = (L:xd) } = do
  l <- btLeft p
  return $ Zipper l BT{ btValue = btValue p, btLeft = Just f, btRight = btRight p } xd

setValue :: a -> Zipper a -> Zipper a
setValue val zp = zp{ focus = (focus zp){ btValue = val } }

setLeft :: Maybe (BinTree a) -> Zipper a -> Zipper a
setLeft tree zp = zp{ focus = (focus zp){ btLeft = tree } }

setRight :: Maybe (BinTree a) -> Zipper a -> Zipper a
setRight tree zp = zp{ focus = (focus zp){ btRight = tree } }
