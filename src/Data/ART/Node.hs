{-# LANGUAGE GADTs #-}

module Data.ART.Node where

import Data.Word
import Data.IORef
import Control.Exception
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

 -- In this implementation, nodes contain a fixed-length vector consisting of the first N bytes of any compressed prefix.
maxPrefixSize :: Int
maxPrefixSize = 8

-- Nodes should have slightly different implementations
-- Node4: equally sized vectors of keys and pointers
-- Node16: (no) like Node4 but key checking in paper used SIMD tricks I don't want to try to replicate, lol
-- Node48: Length 48 vector of sorted keys, length 256 vector of pointers indexed by key instead of index of key in key vector
-- Node256: Single vector of just pointers, indexed by key
data Node a = Node4 { numKeys :: (IORef Word8), partialKeys :: !(UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: !Word8, prefix :: !(UMV.IOVector Word8) } |
              Node16 { numKeys :: (IORef Word8), partialKeys :: !(UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: !Word8, prefix :: !(UMV.IOVector Word8) } |
              Node48 { numKeys :: (IORef Word8), partialKeys :: !(UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: !Word8, prefix :: !(UMV.IOVector Word8) } |
              Node256 { numKeys :: (IORef Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: !Word8, prefix :: !(UMV.IOVector Word8) } |
              Leaf BS.ByteString a |
              Empty

instance Show (Node a) where
    show Empty = "Empty"
    show (Leaf _ _ ) = "Leaf"
    show (Node4 _ _ _ _ _ ) = "Node4"
    show (Node16 _ _ _ _ _ ) = "Node16"
    show (Node48 _ _ _ _ _ ) = "Node48"
    show (Node256 _ _ _ _ ) = "Node256"

newNode4 :: IO (Node a)
newNode4 = do
  k <- UMV.replicate 4 0
  v <- MV.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  c <- newIORef (0 :: Word8)
  return $ Node4 c k v 0 p

newNode16 :: IO (Node a)
newNode16 = do
  k <- UMV.replicate 16 0
  v <- MV.replicate 16 Empty
  p <- UMV.new maxPrefixSize
  c <- newIORef (0 :: Word8)
  return $ Node16 c k v 0 p

newNode48 :: IO (Node a)
newNode48 = do
  k <- UMV.replicate 48 0
  v <- MV.replicate 256 Empty
  p <- UMV.new maxPrefixSize
  c <- newIORef (0 :: Word8)
  return $ Node48 c k v 0 p

newNode256 :: IO (Node a)
newNode256 = do
  v <- MV.replicate 256 Empty
  p <- UMV.new maxPrefixSize
  c <- newIORef (0 :: Word8)
  return $ Node256 c v 0 p

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
itemIs :: UMV.IOVector Word8 -> Int -> Word8 -> IO Bool
itemIs v i k = do
  item <- UMV.read v i
  return $ item == k

hasNumKeys :: Node a -> Int -> IO Bool
hasNumKeys n k = do
  nK <- readIORef $ numKeys n
  return $ (fromIntegral nK) == k

-- TODO: Make sure this actually works as expected
shouldShrink :: Node a -> IO Bool
shouldShrink Empty = return False
shouldShrink (Leaf _ _) = return False
shouldShrink n@(Node4 _ pk _ _ _) = hasNumKeys n 1
shouldShrink n@(Node16 _ pk _ _ _) = hasNumKeys n 4
shouldShrink n@(Node48 _ pk _ _ _) = hasNumKeys n 16
shouldShrink n@(Node256 _ _ _ _) = hasNumKeys n 48

isFull :: Node a -> IO Bool
isFull Empty = return True
isFull (Leaf _ _) = undefined
isFull n@(Node4 _ _ _ _ _) = hasNumKeys n 4
isFull n@(Node16 _ _ _ _ _) = hasNumKeys n 16
isFull n@(Node48 _ _ _ _ _) = hasNumKeys n 48
isFull n@(Node256 _ _ _ _) = hasNumKeys n 256

{-# NOINLINE keyIndex #-}
keyIndex :: Node a -> Word8 -> IO (Maybe Int)
keyIndex Empty _ = undefined
keyIndex (Leaf _ _) _ = undefined
keyIndex n@(Node256 _ _ _ _) k = return $ return $ fromIntegral k
keyIndex node key = do
    nK <- readIORef $ numKeys node
    keys <- UV.unsafeFreeze $ UMV.take (fromIntegral nK) $ partialKeys node
    return $ UV.findIndex (== key) keys

getIxN :: UMV.IOVector Word8 -> Word8 -> Word8 -> IO (Maybe Int)
getIxN vec key nK = go 0
  where go ix = if ix == (fromIntegral nK) then return Nothing else do
          v <- UMV.read vec ix
          if v == key then return (Just ix) else go (ix + 1)

getIx :: UMV.IOVector Word8 -> Word8 -> IO (Maybe Int)
getIx vec key = go 0
  where go ix = if ix == (UMV.length vec) then return Nothing else do
          v <- UMV.read vec ix
          if v == key then return (Just ix) else go (ix + 1)

getKey :: Node a -> Int -> IO Word8
getKey Empty _ = undefined
getKey (Leaf _ _) _ = undefined
getKey (Node256 _ p _ _) i = return $ fromIntegral i
getKey node i = UMV.read (partialKeys node) i
 
maybeGetChild :: Node a -> Word8 -> IO (Maybe (Node a))
maybeGetChild Empty _ = return Nothing
maybeGetChild (Leaf _ _) _ = return Nothing
maybeGetChild (Node256 _ pt _ _) k = do
  c <- MV.read pt (fromIntegral k)
  return $ Just c
maybeGetChild node@(Node48 c k pt pl p) key = do
  c <- MV.read pt (fromIntegral key)
  return $ Just c
maybeGetChild node key = do
  ix <- keyIndex node key
  case (ix) of
    Nothing -> return $ Nothing
    Just i -> do
      c <- catch (MV.read (pointers node) i) (\e -> do
        print $ show (e :: ErrorCall)
        return Empty)
      return $ Just c

insertKey :: UMV.IOVector Word8 -> Word8 -> Word8 -> IO (Word8, Bool)
insertKey keys key 0 = do
  UMV.write keys 0 key
  return (0, True)
insertKey keys key lim = do
    frzKeys <- UV.freeze keys
    let (relevant, remainder) = UV.splitAt (fromIntegral lim) $ frzKeys
    let (prefix, suffix) = UV.span (\x -> x < key) relevant
    if (UV.length suffix > 0 && UV.head suffix == key) then
        return (fromIntegral $ UV.length prefix, False)
    else do
        let insertedIx = fromIntegral $ UV.length prefix
        let suff = if UV.length remainder > 0 then suffix UV.++ (UV.init remainder) else suffix
        newKeys <- UV.thaw $ (UV.snoc prefix key) UV.++ suff
        UMV.copy keys newKeys
        return (insertedIx, True)

insertChildAt :: MV.IOVector (Node a) -> Int -> Node a -> IO ()
insertChildAt ptrs 0 child = do
  frzPtrs <- V.freeze ptrs
  newChildren <- V.thaw $ V.init $ V.cons child frzPtrs
  MV.copy ptrs newChildren
insertChildAt ptrs ix child = do
  frzChildren <- V.freeze ptrs
  let (prefix, suffix) = V.splitAt ix frzChildren
  newChildren <- V.thaw $ V.init $ V.concat [prefix, pure child, suffix]
  MV.copy ptrs newChildren

setChild :: Node a -> Word8 -> Node a -> IO () -- Assumes not full, will be different for each node type
setChild Empty _ _ = undefined
setChild (Leaf _ _ ) _ _ = undefined
setChild parent@(Node256 _ p _ _) key child = do
  c <- MV.read p (fromIntegral key)
  case c of
    Empty -> do
      modifyIORef (numKeys parent) (+1)
      MV.write p (fromIntegral key) child
    _ -> MV.write p (fromIntegral key) child
setChild parent@(Node48 c k p _ _) key child = do
  ct <- readIORef c
  (ix, new) <- insertKey k key ct
  case new of
    False -> MV.write p (fromIntegral key) child
    True -> do
      modifyIORef (numKeys parent) (+1)
      MV.write p (fromIntegral key) child
setChild parent key child = do
  ct <- readIORef $ numKeys parent
  (ix, new) <- insertKey (partialKeys parent) key ct
  case new of
    False -> MV.write (pointers parent) (fromIntegral ix) child
    True -> do
      modifyIORef (numKeys parent) (+1)
      insertChildAt (pointers parent) (fromIntegral ix) child

removeIx :: MV.IOVector a -> Int -> a -> IO ()
removeIx vec index e = do
  let remain = MV.drop (index + 1) vec
  let target = MV.init $ MV.drop index vec
  MV.move target remain
  MV.write vec ((MV.length vec) - 1) e

removeUIx :: UMV.IOVector Word8 -> Int -> Word8 -> IO ()
removeUIx vec index e = do
  let remain = UMV.drop (index + 1) vec
  let target = UMV.init $ UMV.drop index vec
  UMV.move target remain
  UMV.write vec ((UMV.length vec) - 1) e

unsetChildNoCopy :: Node a -> Word8 -> IO ()
unsetChildNoCopy Empty _ = return ()
unsetChildNoCopy (Leaf _ _) _ = return ()
unsetChildNoCopy node key = do
  nK <- readIORef $ numKeys node
  maybeIndex <- getIx (UMV.take (fromIntegral nK) $ partialKeys node) key
  case maybeIndex of
    Nothing -> return ()
    Just index -> if index > 0 && key == (0 :: Word8) then return () else do
      removeIx (pointers node) index Empty
      removeUIx (partialKeys node) index 0

unsetChild :: Node a -> Word8 -> IO ()
unsetChild Empty _ = return ()
unsetChild (Leaf _ _) _ = return ()
unsetChild (Node256 n p _ _) key = do
  c <- MV.read p (fromIntegral key)
  case c of
    Empty -> return ()
    _ -> do
      modifyIORef n (\x -> x - 1)
      MV.write p (fromIntegral key) Empty
unsetChild node@(Node48 n k p _ _) key = do
  c <- MV.read p (fromIntegral key)
  ix <- keyIndex node key
  case c of
    Empty -> return ()
    _ -> do
      modifyIORef n (\x -> x - 1)
      MV.write p (fromIntegral key) Empty
  case ix of
    Nothing -> return ()
    Just keyIndex -> do
      keysLength <- (readIORef n) >>= return . fromIntegral
      modifyIORef (numKeys node) (\x -> x - 1)
      -- Remove and shift keys
      mapM_ (\i -> if i < keyIndex
                    then return ()
                    else if i == keysLength - 1
                        then UMV.write (partialKeys node) i (0 :: Word8)
                        else (UMV.read (partialKeys node) (i + 1)) >>= (\k -> UMV.write (partialKeys node) i k)
            ) [0..(keysLength - 1)]
      return ()
unsetChild node key = do
  ix <- keyIndex node key
  case ix of
    Nothing -> return ()
    Just keyIndex -> do
      let keysLength = UMV.length $ partialKeys node
      modifyIORef (numKeys node) (\x -> x - 1)
      -- Remove and shift keys
      mapM_ (\i -> if i < keyIndex
                    then return ()
                    else if i == keysLength - 1
                        then UMV.write (partialKeys node) i (0 :: Word8)
                        else (UMV.read (partialKeys node) (i + 1)) >>= (\k -> UMV.write (partialKeys node) i k)
            ) [0..(keysLength - 1)]
      -- Remove and shift pointers
      mapM_ (\i -> if i < keyIndex
                    then return ()
                    else if i == keysLength - 1
                        then MV.write (pointers node) i Empty
                        else (MV.read (pointers node) (i + 1)) >>= (\k -> MV.write (pointers node) i k)
            ) [0..(keysLength - 1)]
      return ()

superAddChild :: Node a -> Word8 -> Node a -> IO (Node a) -- Returns node (useful for if growth must occur)
superAddChild parent key child = do
    full <- wouldBeFull parent key
    case full of
        False -> do -- No need to grow node
            setChild parent key child
            return parent
        True -> do -- Grow the node!
            newParent <- growNode parent
            setChild newParent key child
            return newParent

growNode :: Node a -> IO (Node a)
growNode Empty = newNode4
growNode (Leaf k l) = newNode4
growNode (Node4 n pk po pl p) = do
  nPk <- UMV.grow pk 12
  nP <- MV.grow po 12
  return $ Node16 n nPk nP pl p
growNode (Node16 _n pk po pl p) = do --special, need to re-order child vector
  _newNode <- newNode48
  let newNode = _newNode{ numKeys = _n, prefixLen = pl, prefix = p }
  UMV.copy (UMV.take 16 $ partialKeys newNode) pk
  mapM_ (\i -> do
      theKey <- UMV.read pk i
      theChild <- MV.read po i
      MV.write (pointers newNode) (fromIntegral theKey) theChild
    ) [0..15]
  return newNode
growNode (Node48 n pk po pl p) = do
  return $ Node256 n po pl p
growNode n@(Node256 _ _ _ _) = return n -- ???

shrinkNode :: Node a -> IO (Node a)
shrinkNode Empty = return Empty
shrinkNode (Leaf k l) = return Empty
shrinkNode (Node4 n pk po pl p) = do
  val <- MV.read po 0 -- Could check if this is definitely a leaf, but it should be in this case
  return $ val
shrinkNode (Node16 n pk po pl p) = do
  let nPk = UMV.unsafeTake 4 pk
  let nP = MV.unsafeTake 4 po
  return $ Node4 n nPk nP pl p
shrinkNode (Node48 n pk po pl p) = do
  let nPk = UMV.unsafeTake 16 pk
  nP <- MV.replicate 16 Empty
  mapM_ (\i -> do
      theKey <- UMV.read nPk i
      theChild <- MV.read po (fromIntegral theKey)
      MV.write nP (fromIntegral theKey) theChild
    ) [0..(15)]
  return $ Node16 n nPk nP pl p
shrinkNode (Node256 n po pl p) = do
  new48 <- newNode48
  mapM_ (\i -> do
      theChild <- MV.read po i
      case theChild of
        Empty -> return ()
        _ -> setChild new48 (fromIntegral i) theChild
    ) [0..255]
  return new48
-- Indicate if node would be full if we attempted to insert a thing
-- Must be both full and not contain the key
wouldBeFull :: Node a -> Word8 -> IO Bool
wouldBeFull Empty _ = return False
wouldBeFull (Leaf _ _ ) _ = return True
wouldBeFull n k = do
  full <- isFull n
  notContainsKey <- (keyIndex n k) >>= (\x -> return $ x == Nothing)
  return $ full && notContainsKey

-- Depth sort of matters for leafMatches, but I'm not sure of the best way to
-- switch between hybrid (only check subsequent to depth) and optimistic (check all)
-- modes. Defaulting to optimistic for now.
leafMatches :: Node a -> BS.ByteString -> Int -> Bool
leafMatches (Leaf k _) key _ = if k == key then True else False

checkPrefix :: Node a -> BS.ByteString -> Int -> IO Int
checkPrefix Empty _ _ = return 0
checkPrefix (Leaf leafKey _) key depth = do
  let relevantKey = BS.unpack $ BS.drop depth key
  let relevantLeafKey = BS.unpack $ BS.drop depth leafKey
  return $ (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) relevantKey relevantLeafKey
checkPrefix node k depth = if (prefixLen node == 0) then return 0 else do
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  nodePrefix <- UV.freeze $ UMV.take prefixLength (prefix node)
  let activeKey = BS.take prefixLength $ BS.drop depth k -- Is this right? drop to depth, pick up prefixLen length?
  let matches = (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) (UV.toList nodePrefix) (BS.unpack activeKey)
  return matches

resizePrefix :: UMV.IOVector Word8 -> Int -> IO (UMV.IOVector Word8)
resizePrefix p len = do
  newVec <- UMV.replicate len 0
  UMV.copy (UMV.take (UMV.length p) newVec) p
  return newVec

printNode :: (Show a) => Node a -> IO ()
printNode Empty = print "Empty"
printNode (Leaf k v) = do
  print "Leaf:"
  print k
  print v
  print "  "
printNode n = do
    print "encountered node:"
    printUMVVal $ partialKeys n
    printVal $ pointers n
  where printUMVVal v = if (UMV.length v == 0) then return () else do
          val <- UMV.read v 0
          print $ "key: " ++ (show val)
          printUMVVal $ UMV.tail v
        printVal v = if (MV.length v == 0) then return () else do
          val <- MV.read v 0
          print "found value:"
          printNode val
          printVal $ MV.tail v