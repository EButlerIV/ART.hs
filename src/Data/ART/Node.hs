module Data.ART.Node where

import Data.Word
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

-- In the paper, Node16 does special SIMD stuff, but I haven't implemented that yet.
-- Node48 has a key vector size 48, but child vector size 256.
-- Node256 has no key vector, as child vector is size of key space.
data Node a = Node4 { keyCount :: Word8, partialKeys :: (UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Node16 { keyCount :: Word8, partialKeys :: (UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Node48 { keyCount :: Word8, partialKeys :: (UMV.IOVector Word8), pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Node256 { keyCount :: Word8, pointers :: (MV.IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Word8) } |
              Leaf BS.ByteString a |
              Empty

instance Show (Node a) where
    show Empty = "Empty"
    show (Leaf _ _ ) = "Leaf"
    show (Node4 _ _ _ _ ) = "Node4"
    show (Node16 _ _ _ _ ) = "Node16"
    show (Node48 _ _ _ _ ) = "Node48"
    show (Node256 _ _ _ _ ) = "Node256"

newNode4 :: IO (Node a)
newNode4 = do
  k <- UMV.new 4
  v <- MV.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  return $ Node4 0 k v 0 p

newNode16 :: IO (Node a)
newNode16 = do
  k <- UMV.new 16
  v <- MV.replicate 16 Empty
  p <- UMV.new maxPrefixSize
  return $ Node16 0 k v 0 p

newNode48 :: IO (Node a)
newNode48 = do
  k <- UMV.new 48
  v <- MV.replicate 48 Empty
  p <- UMV.new maxPrefixSize
  return $ Node48 0 k v 0 p

newNode256 :: IO (Node a)
newNode256 = do
  v <- MV.replicate 256 Empty
  p <- UMV.new maxPrefixSize
  return $ Node256 0 v 0 p

-- | getChild - Retrieves child from node for the given key, if it exists
-- Question: Is it worth it to keep the maybe, or acceptable to return an Empty node in all not found cases?
getChild :: Node a -> Word8 -> IO (Maybe (Node a))
getChild Empty _ = return Nothing
getChild (Leaf _ _) _ = return Nothing
getChild (Node256 numChildren children _ _) key = if numChildren == 0 then return Nothing else do
  c <- MV.read children (fromIntegral key)
  if c == Empty then return Nothing else return (Just c)
getChild node@(Node48 numChildren keys children _ _) key = if numChildren == 0 then return Nothing else do
  ix <- keyIndex node key -- TODO: superfluous index read, just return child 
  case (ix) of
    Nothing -> return $ Nothing
    Just i -> do
      c <- MV.read children (fromIntegral key)
      if c == Empty then return Nothing else return (Just c)
getChild node key = do
    ix <- keyIndex node key
    case (ix) of
      Nothing -> return $ Nothing
      Just i -> do
        c <- MV.read (pointers node) i
        return $ Just c

-- | setChild adds node as child of other node, will overwrite previous child if key is the same.
-- | It assumes the node is not full.
setChild :: Node a -> Word8 -> Node a -> IO ()
setChild Empty _ _ = undefined
setChild (Leaf _ _ ) _ _ = undefined
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
            return ()
        Just i -> do
            MV.write (pointers parent) i child

-- | removeChild checks to see if key is present in node, deletes child if key exists, returns if key existed
removeChild :: Node a -> Word8 -> IO Bool
removeChild Empty _ = undefined
removeChild (Leaf _ _) = undefined
removeChild (Node256 ct children _ _ _) = do
  c <- MV.read children (fromIntegral key)
  if c == Empty then return False else do
    

shouldShrink :: Node a -> IO Bool
shouldShrink Empty = return False
shouldShrink (Leaf _ _) = return False
shouldShrink (Node4 ct _ _ _ _) = ct < 2
shouldShrink (Node16 ct _ _ _ _) = ct < 5
shouldShrink (Node48 ct _ _ _ _) = ct < 17
shouldShrink (Node256 ct _ _ _) = ct < 49

isFull :: Node a -> IO Bool
isFull Empty = return True
isFull (Leaf _ _) = undefined
isFull n = do
  let keysLen = UMV.length $ partialKeys n
  k <- catch (UMV.read (partialKeys n) (keysLen - 1)) (\e -> do
    print $ show (e :: ErrorCall)
    return 0)
  return $ k /= 0

keyIndex :: Node a -> Word8 -> IO (Maybe Int) -- TODO: Use binary search maybe. Not sure of the performance implications when arrays so short
keyIndex node key = do
    keys <- UV.freeze $ partialKeys node
    let ix = UV.findIndex (== key) keys
    case ix of
        Nothing -> return Nothing
        Just i -> return $ if key == 0 && i /= 0 then Nothing else (Just i)

-- maybeGetChild :: Node a -> Word8 -> IO (Maybe (Node a))
-- maybeGetChild node key = do
--   ix <- keyIndex node key
--   case (ix) of
--     Nothing -> return $ Nothing
--     Just i -> do
--       -- print $ "found index: " ++ (show i)
--       c <- MV.read (pointers node) i
--       -- print $ "found child? " ++ (show c)
--       return $ Just c


-- addChild :: Node a -> Word8 -> Node a -> IO ()
-- addChild parent key child = do
--     -- print "adding to node"
--     (UV.freeze $ partialKeys parent) >>= (print)
--     parentKeys <- mapM (\i -> UMV.read (partialKeys parent) i) [0..(keysLength - 1)]
--     children <- mapM (\i -> MV.read (pointers parent) i) [0..(keysLength - 1)]
--     let (f, s) = L.splitAt 1 $ ((zip parentKeys children) ++ [(key, child)])
--     let pairs = f ++ (L.sortOn fst $ filter (\(x, _) -> x /= 0) s)
--     mapM_ (\i -> do
--                     print $ "writing index " ++ (show i) ++ " " ++ (show $ fst $ pairs !! i)
--                     UMV.write (partialKeys parent) i (fst $ pairs !! i)
--                     MV.write (pointers parent) i (snd $ pairs !! i)
--           ) [0..(L.length pairs) - 1]
--     return ()
--   where keysLength = UMV.length (partialKeys parent)

            

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
growNode (Node4 pk po pl p) = do
  nPk <- UMV.grow pk 12
  nP <- MV.grow po 12
  return $ Node16 nPk nP pl p
growNode (Node16 pk po pl p) = do
  nPk <- UMV.grow pk 32
  nP <- MV.grow po 32
  return $ Node48 nPk nP pl p
growNode (Node48 pk po pl p) = do
  nPk <- UMV.grow pk 208
  nP <- MV.grow po 208
  return $ Node256 nPk nP pl p
growNode n@(Node256 pk po pl p) = return n

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
  -- print $ "checkPrefix matches " ++ (show matches)
  -- print $ show $ nodePrefix
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