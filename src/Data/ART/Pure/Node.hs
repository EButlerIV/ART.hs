{-# LANGUAGE GADTs #-}

module Data.ART.Pure.Node where


import Data.Word
import Control.Monad.ST
import Data.Primitive.SmallArray
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS

maxPrefixSize :: Int
maxPrefixSize = 8

data Node a = Node4 { numKeys :: Word8, partialKeys :: !BSS.ShortByteString, pointers :: (SmallArray (Node a)), prefixLen :: !Word8, prefix :: !BSS.ShortByteString } |
              Node16 { numKeys :: Word8, partialKeys :: !BSS.ShortByteString, pointers :: (SmallArray (Node a)), prefixLen :: !Word8, prefix :: !BSS.ShortByteString } |
              Node48 { numKeys :: Word8, partialKeys :: !BSS.ShortByteString, pointers :: (SmallArray (Node a)), prefixLen :: !Word8, prefix :: !BSS.ShortByteString } |
              Node256 { numKeys :: Word8, pointers :: (SmallArray (Node a)), prefixLen :: !Word8, prefix :: !BSS.ShortByteString } |
              Leaf BS.ByteString a |
              Empty deriving (Eq, Show)

-- instance Show (Node a) where
--     show Empty = "Empty"
--     show (Leaf k v ) = "Leaf " ++ (show k)
--     show (Node4 c k pt pl p ) = "Node4 " ++ (show c) ++ " " ++ (show k) ++ " " ++ (show pt) ++ " " ++ (show pl) ++ " " ++ (show p)
--     show (Node16 c k pt pl p ) = "Node16 " ++ (show c) ++ " " ++ (show k) ++ " " ++ (show pt) ++ " " ++ (show pl) ++ " " ++ (show p)
--     show (Node48 _ _ _ _ _ ) = "Node48"
--     show (Node256 _ _ _ _ ) = "Node256"

isEmpty :: Node a -> Bool
isEmpty Empty = True
isEmpty _ = False

isNode16 :: Node a -> Bool
isNode16 (Node16 _ _ _ _ _) = True
isNode16 _ = False

isNode48 :: Node a -> Bool
isNode48 (Node48 _ _ _ _ _) = True
isNode48 _ = False

isNode256 :: Node a -> Bool
isNode256 (Node256 _ _ _ _) = True
isNode256 _ = False

newNodeStuff :: Int -> (BSS.ShortByteString, SmallArray (Node a), BSS.ShortByteString)
newNodeStuff len = (keys, children, prefix)
    where keys = BSS.toShort $ BS.replicate len 0
          children = smallArrayFromList $ replicate len Empty
          prefix = BSS.toShort $ BS.replicate maxPrefixSize 0

newNode4 :: Node a
newNode4 = Node4 0 keys children 0 prefix
    where (keys, children, prefix) = newNodeStuff 4

newNode16 :: Node a
newNode16 = Node16 0 keys children 0 prefix
    where (keys, children, prefix) = newNodeStuff 16

newNode48 :: Node a
newNode48 = Node48 0 keys children 0 prefix
    where keys = BSS.toShort $ BS.replicate 48 0
          children = smallArrayFromList $ replicate 256 Empty
          prefix = BSS.toShort $ BS.replicate maxPrefixSize 0

newNode256 :: Node a
newNode256 = Node256 0 children 0 prefix
    where children = smallArrayFromList $ replicate 256 Empty
          prefix = BSS.toShort $ BS.replicate maxPrefixSize 0


shouldShrink :: Node a -> Bool
shouldShrink Empty = False
shouldShrink (Leaf _ _) = False
shouldShrink (Node4 c _ _ _ _) = c == 1
shouldShrink (Node16 c _ _ _ _) = c == 4
shouldShrink (Node48 c _ _ _ _) = c == 16
shouldShrink (Node256 c _ _ _) = c == 48

isFull :: Node a -> Bool
isFull Empty = True
isFull (Leaf _ _) = True
isFull (Node4 c _ _ _ _) = c == 4
isFull (Node16 c _ _ _ _) = c == 16
isFull (Node48 c _ _ _ _) = c == 48
isFull (Node256 c _ _ _) = False

wouldBeFull :: Node a -> Word8 -> Bool
wouldBeFull node key = isFull node && (keyIndex node key) == Nothing
-- Put growNode here!

resizePointers :: SmallArray (Node a) -> Int -> SmallArray (Node a)
resizePointers ptrs len = runST $ do
    let s = sizeofSmallArray ptrs
    mp <- newSmallArray len Empty
    copySmallArray mp 0 ptrs 0 s
    freezeSmallArray mp 0 len

resizePrefix :: BSS.ShortByteString -> Int -> BSS.ShortByteString
resizePrefix p i = BSS.pack $ (BSS.unpack p) ++ padding
    where padding = replicate (max (i - (BSS.length p)) 0) 0

growNode :: Node a -> Node a
growNode Empty = undefined
growNode (Leaf _ _) = undefined
growNode n@(Node256 _ _ _ _) = undefined
growNode n@(Node4 c k pt pl p) = newNode16{ numKeys = c, partialKeys = newKeys, pointers = newPointers, prefixLen = pl, prefix = p }
    where newKeys = BSS.toShort $ BS.append (BSS.fromShort k) (BS.replicate 12 0)
          newPointers = resizePointers pt 16
-- growNode n@(Node4 c keys pt pl p) = newNode{ prefixLen = pl, prefix = p }
--     where newNode = foldl (\n (k, i) -> setChild n k (indexSmallArray pt i)) newNode16 (zip (BSS.unpack keys) [0..3])
growNode n@(Node16 c k pt pl p) = newNode
    where _newNode = newNode48{ prefixLen = pl, prefix = p }
          newNode = foldl (\n (k, i) -> setChild n k (indexSmallArray pt i)) _newNode (zip (BSS.unpack k) [0..])
growNode (Node48 c k pt pl p) = Node256 c pt pl p

shrinkNode :: Node a -> Node a
shrinkNode Empty = Empty
shrinkNode (Leaf _ _) = undefined
shrinkNode n@(Node256 c pt pl p) = Node48 c k pt pl p
    where pairs = map (\i -> (i, indexSmallArray pt i)) [0..255]
          _k = BS.pack $ foldl (\keys (ix, c) -> if isEmpty c then keys else keys ++ [fromIntegral ix]) [] pairs
          k = BSS.toShort $ BS.append _k (BS.replicate (256 - BS.length _k) 0)
shrinkNode (Node48 c k pt pl p) = newNode
    where _newNode = newNode16{ numKeys = c, prefixLen = pl, prefix = p }
          newNode = foldl (\n k -> setChild n k (indexSmallArray pt (fromIntegral k))) _newNode (BSS.unpack k)
shrinkNode (Node16 c k pt pl p) = Node4 c newKeys newPointers pl p
    where newKeys = BSS.toShort $ BS.take 4 $ BSS.fromShort k
          newPointers = cloneSmallArray pt 0 4 
shrinkNode (Node4 _ _ pt _ _) = indexSmallArray pt 0

keyIndex :: Node a -> Word8 -> Maybe Int
keyIndex Empty _ = Nothing
keyIndex (Leaf _ _) _ = Nothing
keyIndex (Node256 _ pt _ _) k = case indexSmallArray pt (fromIntegral k) of
    Empty -> Nothing
    _ -> return $ fromIntegral k
keyIndex node key = BS.elemIndex key keys
    where keys = BS.take (fromIntegral $ numKeys node) $ BSS.fromShort $ partialKeys node
    

-- TODO: Maybe shouldn't be maybe. Could just return Empties?
maybeGetChild :: Node a -> Word8 -> Maybe (Node a)
maybeGetChild Empty _ = Nothing
maybeGetChild (Leaf _ _) _ = Nothing
maybeGetChild (Node256 _ pt _ _) k = Just $ indexSmallArray pt (fromIntegral k)
maybeGetChild node@(Node48 c k pt pl p) key = case keyIndex node key of
    Nothing -> Nothing
    Just i -> return $ indexSmallArray pt (fromIntegral key)
maybeGetChild node key = do
    ix <- keyIndex node key
    return $ indexSmallArray (pointers node) ix


insertKey :: BSS.ShortByteString -> Word8 -> Word8 -> (BSS.ShortByteString, Word8, Bool)
insertKey keys key 0 = do
    let k = BS.init $ BSS.fromShort keys
    let newKeys = BSS.toShort $ BS.cons key k
    (BSS.toShort $ BS.init $ BS.cons key $ BSS.fromShort keys, 0, True)
insertKey keys key lim = do
    let (relevant, remainder) = BS.splitAt (fromIntegral lim) $ BSS.fromShort keys
    let (prefix, suffix) = BS.span (\x -> x < key) relevant
    if (BS.length suffix > 0 && BS.head suffix == key) then
        (keys, fromIntegral $ BS.length prefix, False)
    else do
        let insertedIx = fromIntegral $ BS.length prefix
        let suff = if BS.length remainder > 0 then BS.append suffix (BS.init remainder) else suffix
        (BSS.toShort $ BS.append (BS.snoc prefix key) suff, insertedIx, True)

removeKey :: BSS.ShortByteString -> Word8 -> Word8 -> (BSS.ShortByteString, Maybe Word8)
removeKeys keys _ 0 = (keys, Nothing)
removeKey keys key lim = if BS.length pref == BSS.length keys then (keys, Nothing) else (BSS.toShort $ BS.append (BS.append pref (BS.snoc (BS.tail suff) 0)) rem, Just $ fromIntegral $ BS.length pref)
    where   (bs, rem) = BS.splitAt (fromIntegral lim) $ BSS.fromShort keys
            (pref, suff) = BS.span (\x -> x /= key) bs

insertChild :: SmallArray (Node a) -> Word8 -> Node a -> SmallArray (Node a)
insertChild ptrs ix child = runST $ do
    let s = sizeofSmallArray ptrs
    mp <- thawSmallArray ptrs 0 s
    writeSmallArray mp (fromIntegral ix) child
    freezeSmallArray mp 0 s

insertChildAt :: SmallArray (Node a) -> Int -> Node a -> SmallArray (Node a)
insertChildAt ptrs ix child = runST $ do
    let s = sizeofSmallArray ptrs
    new <- newSmallArray s Empty
    writeSmallArray new ix child
    if ix == 0 then do
        copySmallArray new 1 ptrs 0 (s - 1)
    else do
        let len = min ix s
        copySmallArray new 0 ptrs 0 len
        let destinationOffset = min (ix + 1) s
        let sourceOffset = min ix s
        let length = max (s - ix - 1) 0
        copySmallArray new destinationOffset ptrs sourceOffset length
    freezeSmallArray new 0 s

removeChild :: SmallArray (Node a) -> Word8 -> SmallArray (Node a)
removeChild ptrs ix = insertChild ptrs ix Empty

removeChildAt :: SmallArray (Node a) -> Int -> SmallArray (Node a)
removeChildAt ptrs ix = runST $ do
    let s = sizeofSmallArray ptrs
    mp <- thawSmallArray ptrs 0 s
    let destinationOffset = min ix s
    let sourceOffset = min (ix + 1) s
    let len = max (s - sourceOffset) 0
    copySmallArray mp destinationOffset ptrs sourceOffset len
    writeSmallArray mp (s - 1) Empty
    freezeSmallArray mp 0 s

-- Assumes not full
setChild :: Node a -> Word8 -> Node a -> Node a
setChild Empty _ _ = undefined
setChild (Leaf _ _ ) _ _ = undefined
setChild n@(Node256 c p _ _) key child = n{ numKeys = newNumKeys, pointers = newPointers }
    where newNumKeys = if isEmpty $ indexSmallArray p (fromIntegral key) then c + 1 else c
          newPointers = insertChild p key child    
setChild n@(Node48 c k p _ _) key child = n{ numKeys = newNumKeys, partialKeys = newKeys, pointers = newPointers }
    where (newKeys, _, _) = insertKey k key c
          newNumKeys = if isEmpty $ indexSmallArray p (fromIntegral key) then c + 1 else c
          newPointers = insertChild p key child 
setChild node key child = node{ numKeys = newNumKeys, partialKeys = newKeys, pointers = newPointers }
    where (newKeys, ix, new) = insertKey (partialKeys node) key (numKeys node)
          newPointers = case new of
                False -> insertChild (pointers node) (fromIntegral ix) child
                True -> insertChildAt (pointers node) (fromIntegral ix) child 
          newNumKeys = foldl (\m i -> if isEmpty $ indexSmallArray newPointers i then m else m + 1) 0 [0..((sizeofSmallArray newPointers) - 1)]
            --if isEmpty $ indexSmallArray newPointers (min (fromIntegral $ numKeys node) ((sizeofSmallArray newPointers) - 1)) then (numKeys node) else (fromIntegral  (numKeys node) + 1)
          
unsetChild :: Node a -> Word8 -> Node a
unsetChild Empty _ = undefined
unsetChild (Leaf _ _) _ = undefined
unsetChild n@(Node256 c p _ _) key = n{ numKeys = newNumKeys, pointers = newPointers }
    where newNumKeys = if isEmpty $ indexSmallArray p (fromIntegral key) then c else c - 1
          newPointers = removeChild p key
unsetChild n@(Node48 c k p _ _) key = n{ numKeys = newNumKeys, partialKeys = newKeys, pointers = newPointers }
    where (newKeys, mIx) = removeKey k key c
          newPointers = removeChild p key
          newNumKeys = if isEmpty $ indexSmallArray p (fromIntegral key) then c else c - 1
unsetChild node key = node{ numKeys = newNumKeys, partialKeys = newKeys, pointers = newPointers }
    where (newKeys, mIx) = removeKey (partialKeys node) key (numKeys node)
          (newPointers, newNumKeys) = case mIx of
            Nothing -> (pointers node, numKeys node)
            Just ix -> (removeChildAt (pointers node) (fromIntegral ix), (numKeys node) - 1)

addChild :: Node a -> Word8 -> Node a -> Node a
addChild node key child = case wouldBeFull node key of
    True -> setChild (growNode node) key child
    False -> setChild node key child

leafMatches :: Node a -> BS.ByteString -> Int -> Bool
leafMatches (Leaf k _) key _ = if k == key then True else False

checkPrefix :: Node a -> BS.ByteString -> Int -> Int
checkPrefix Empty _ _ = 0
checkPrefix (Leaf leafKey _) key depth = matches
        where matches = (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) (BS.unpack $ BS.drop depth leafKey) (BS.unpack $ BS.drop depth key)
checkPrefix node k depth = if (prefixLen node == 0) then 0 else matches
    where prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
          nodePrefix = BS.take prefixLength $ BSS.fromShort $ prefix node
          activeKey = BS.take prefixLength $ BS.drop depth k -- Is this right? drop to depth, pick up prefixLen length?
          matches = (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) (BS.unpack nodePrefix) (BS.unpack activeKey)