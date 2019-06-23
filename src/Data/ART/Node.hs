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

itemIs :: UMV.IOVector Word8 -> Int -> Word8 -> IO Bool
itemIs v i k = do
  item <- UMV.read v i
  return $ item == k

hasNumKeys :: Node a -> Int -> IO Bool
hasNumKeys n k = do
  nK <- readIORef $ numKeys n
  return $ (fromIntegral nK) == k

shouldShrink :: Node a -> IO Bool
shouldShrink Empty = return False
shouldShrink (Leaf _ _) = return False
shouldShrink n@(Node4 _ pk _ _ _) = hasNumKeys n 1
shouldShrink n@(Node16 _ pk _ _ _) = hasNumKeys n 5
shouldShrink n@(Node48 _ pk _ _ _) = hasNumKeys n 17
shouldShrink n@(Node256 _ _ _ _) = hasNumKeys n 49

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
    -- children <- V.unsafeFreeze $ pointers node
    let ix = UV.findIndex (== key) keys
    case ix of
        Nothing -> return Nothing
        Just i -> return $ if key == 0 && i /= 0 then Nothing else (Just i)

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
maybeGetChild node key = do
  ix <- keyIndex node key
  case (ix) of
    Nothing -> return $ Nothing
    Just i -> do
      -- print $ "found index: " ++ (show i)
      c <- catch (MV.read (pointers node) i) (\e -> do
        print $ show (e :: ErrorCall)
        return Empty)
      -- print $ "found child? " ++ (show c)
      return $ Just c


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
setChild parent@(Node48 _ k p _ _) key child = do
  ix <- keyIndex parent key
  case ix of
    Nothing -> modifyIORef (numKeys parent) (+1)
    _ -> return ()
  frzKey <- UV.freeze $ partialKeys parent
  let (prefix, suffix) = UV.span (\x -> if key == 0 then False else x < key && x /= 0) frzKey
  newKeys <- UV.thaw $ UV.concat [prefix, UV.singleton key, suffix]
  UMV.copy k (UMV.init newKeys)
  MV.write p (fromIntegral key) child
setChild parent key child = do
    ix <- keyIndex parent key
    case ix of
        Nothing -> do
            frzKey <- UV.freeze $ partialKeys parent
            frzChildren <- V.freeze $ pointers parent
            let (prefix, suffix) = UV.span (\x -> if key == 0 then False else x < key && x /= 0) frzKey
            let (prefixC, suffixC) = V.splitAt (UV.length prefix) frzChildren
            newKeys <- UV.thaw $ UV.concat [prefix, UV.singleton key, suffix]
            newChildren <- V.thaw $ V.concat [prefixC, pure child, suffixC]
            UMV.copy (partialKeys parent) (UMV.init newKeys)
            MV.copy (pointers parent) (MV.init newChildren)
            modifyIORef (numKeys parent) (+1)
            return ()
        Just i -> do
            MV.write (pointers parent) i child

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
    -- print "adding to node"
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
  -- print "growing node 4"
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
checkPrefix (Leaf leafKey _) key depth = return matches
        where matches = (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) (BS.unpack $ BS.drop depth leafKey) (BS.unpack $ BS.drop depth key)
checkPrefix node k depth = if (prefixLen node == 0) then return 0 else do
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  nodePrefix <- UV.freeze $ UMV.take prefixLength (prefix node)
  let activeKey = BS.take prefixLength $ BS.drop depth k -- Is this right? drop to depth, pick up prefixLen length?
  let matches = (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) (UV.toList nodePrefix) (BS.unpack activeKey)
  return matches

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