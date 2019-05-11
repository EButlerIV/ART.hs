module Data.ART.Node where

import Data.Word
import qualified Data.List as L
import qualified Data.ByteString as BS
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV

 -- In this implementation, nodes contain a fixed-length vector consisting of the first N bytes of any compressed prefix.
maxPrefixSize :: Int
maxPrefixSize = 8

data Node a = Node4 { partialKeys :: (UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Node16 { partialKeys :: (UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Node48 { partialKeys :: (UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Node256 { partialKeys :: (UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Leaf BS.ByteString a |
              Empty

newNode4 :: IO (Node a)
newNode4 = do
  k <- UMV.new 4
  v <- MV.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  return $ Node4 k v 0 p

newNode16 :: IO (Node a)
newNode16 = do
  k <- UMV.new 16
  v <- MV.replicate 16 Empty
  p <- UMV.new maxPrefixSize
  return $ Node16 k v 0 p

newNode48 :: IO (Node a)
newNode48 = do
  k <- UMV.new 48
  v <- MV.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  return $ Node48 k v 0 p

newNode256 :: IO (Node a)
newNode256 = do
  k <- UMV.new 256
  v <- MV.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  return $ Node256 k v 0 p

itemIs :: UMV.IOVector Word8 -> Int -> Word8 -> IO Bool
itemIs v i k = do
  item <- UMV.read v i
  return $ item == k

shouldShrink :: Node a -> IO Bool
shouldShrink Empty = return False
shouldShrink (Leaf _ _) = return False
shouldShrink (Node4 pk _ _ _) = (itemIs pk 1 0) >>= return . not
shouldShrink (Node16 pk _ _ _) = (itemIs pk 5 0) >>= return . not
shouldShrink (Node48 pk _ _ _) = (itemIs pk 17 0) >>= return . not
shouldShrink (Node256 pk _ _ _) = (itemIs pk 49 0) >>= return . not

isFull :: Node a -> IO Bool
isFull Empty = return True
isFull (Leaf _ _) = undefined
isFull n = do
  let keysLen = UMV.length $ partialKeys n
  k <- UMV.read (partialKeys n) (keysLen - 1)
  return $ k /= 0

keyIndex :: Node a -> Word8 -> IO (Maybe Int) -- TODO: Use binary search maybe. Not sure of the performance implications when arrays so short
keyIndex node key = do
                    keys <- mapM (\i -> UMV.read keyVector i) [0..(keysLength - 1)]
                    return $ L.elemIndex key keys
  where keyVector = partialKeys node
        keysLength = UMV.length keyVector

maybeGetChild :: Node a -> Word8 -> IO (Maybe (Node a))
maybeGetChild node key = do
  ix <- keyIndex node key
  case (ix) of
    Nothing -> return $ Nothing
    Just i -> do
      c <- MV.read (pointers node) i
      return $ Just c


addChild :: Node a -> Word8 -> Node a -> IO ()
addChild parent key child = do
    parentKeys <- mapM (\i -> UMV.read (partialKeys parent) i) [0..(keysLength - 1)]
    children <- mapM (\i -> MV.read (pointers parent) i) [0..(keysLength - 1)]
    let (f, s) = L.splitAt 1 $ ((zip parentKeys children) ++ [(key, child)])
    let pairs = f ++ (L.sortOn fst $ filter (\(x, _) -> x /= 0) s)
    mapM_ (\i -> do
                    print $ "writing index " ++ (show i) ++ " " ++ (show $ fst $ pairs !! i)
                    UMV.write (partialKeys parent) i (fst $ pairs !! i)
                    MV.write (pointers parent) i (snd $ pairs !! i)
          ) [0..(L.length pairs) - 1]
    return ()
  where keysLength = UMV.length (partialKeys parent)

growNode :: Node a -> IO (Node a)
growNode Empty = newNode4
growNode (Leaf k l) = newNode4
growNode (Node4 pk po pl p) = do
  nPk <- UMV.unsafeGrow pk 12
  nP <- MV.unsafeGrow po 12
  return $ Node16 nPk nP pl p
growNode (Node16 pk po pl p) = do
  nPk <- UMV.unsafeGrow pk 32
  nP <- MV.unsafeGrow po 32
  return $ Node48 nPk nP pl p
growNode (Node48 pk po pl p) = do
  nPk <- UMV.unsafeGrow pk 208
  nP <- MV.unsafeGrow po 208
  return $ Node256 nPk nP pl p

shrinkNode :: Node a -> IO (Node a)
shrinkNode Empty = return Empty
shrinkNode (Leaf k l) = return Empty
shrinkNode (Node4 pk po pl p) = do
  val <- MV.read po 0 -- Could check if this is definitely a leaf, but it should be in this case
  return $ val
shrinkNode (Node16 pk po pl p) = do
  let nPk = UMV.unsafeTake 4 pk
  let nP = MV.unsafeTake 4 po
  return $ Node4 nPk nP pl p
shrinkNode (Node48 pk po pl p) = do
  let nPk = UMV.unsafeTake 16 pk
  let nP = MV.unsafeTake 16 po
  return $ Node16 nPk nP pl p
shrinkNode (Node256 pk po pl p) = do
  let nPk = UMV.unsafeTake 48 pk
  let nP = MV.unsafeTake 48 po
  return $ Node48 nPk nP pl p
-- Indicate if node would be full if we attempted to insert a thing
-- Must be both full and not contain the key
wouldBeFull :: Node a -> Word8 -> IO Bool
wouldBeFull Empty _ = return False
wouldBeFull (Leaf _ _ ) _ = return True
wouldBeFull n k = do
  full <- isFull n
  containsKey <- (keyIndex n k) >>= (\x -> return $ x /= Nothing)
  return $ full && containsKey

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
  print $ "checkPrefix matches " ++ (show matches)
  print $ show $ nodePrefix
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