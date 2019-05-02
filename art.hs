{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}

module Main where

import Data.Struct.Internal
import Data.Struct.TH
import Data.Struct
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Word
import Data.List as L hiding (insert)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.Array.Unboxed as UA
import qualified Data.Vector.Unboxed as UV
import Data.Vector.Mutable as V
import qualified Data.Vector as IV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Bits (unsafeShiftR)
import Data.Serialize
import qualified Data.ByteString.Char8 as C

-- Based on https://db.in.tum.de/~leis/papers/ART.pdf
-- We therefore use two additional, well-known techniques that allow to decrease
-- the height by reducing the number of nodes.
--  * Lazy Expansion
--    - Nodes only created when necessary to distinguish two children
--    - Refer to key in child to check for match
--  * Path Compression
--    - All inner nodes with single children are removed
--    - Pessimistic: At each inner node, a variable length
--      (possibly empty) partial key vector is stored. It
--      contains the keys of all preceding one-way nodes which
--      have been removed. During lookup this vector is compared to the
--      search key before proceeding to the next child.
--    - Optimistic: Only the count of preceding one-way nodes
--      (equal to the length of the vector in the pessimistic
--      approach) is stored. Lookups just skip this number of
--      bytes without comparing them. Instead, when a lookup
--      arrives at a leaf its key must be compared to the search
--      key to ensure that no “wrong turn” was taken.
--    - HYBRID: Pessimistic lookup on fixed 8-byte array. If gap is longer,
--      fall back to optimistic search

type Key = BS.ByteString
type Byte = Word8

maxPrefixSize :: Int
maxPrefixSize = 8
-- There are four kinds of nodes
-- Partialkeys should be unboxed vector
-- TODO: Node48 and node256 are special (node16 has cool lookup). Implement different structure and abstract behind a typeclass or something, maybe
data Node a = Node4 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Byte) } |
              Node16 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Byte) } |
              Node48 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Byte) } |
              Node256 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a)), prefixLen :: Word8, prefix :: (UMV.IOVector Byte) } |
              Leaf Key a | -- Could/should store key prefix in leaf
              Empty

newNode4 :: IO (Node a)
newNode4 = do
  k <- UMV.new 4
  v <- V.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  return $ Node4 k v 0 p

newNode16 :: IO (Node a)
newNode16 = do
  k <- UMV.new 16
  v <- V.replicate 16 Empty
  p <- UMV.new maxPrefixSize
  return $ Node16 k v 0 p

newNode48 :: IO (Node a)
newNode48 = do
  k <- UMV.new 48
  v <- V.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  return $ Node48 k v 0 p

newNode256 :: IO (Node a)
newNode256 = do
  k <- UMV.new 256
  v <- V.replicate 4 Empty
  p <- UMV.new maxPrefixSize
  return $ Node256 k v 0 p

searchZero :: UMV.IOVector Word8 -> IO Bool
searchZero empty = return False
searchZero v = do
  val <- UMV.read v 0
  if val == 0 then return True else searchZero (UMV.tail v)

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
                    return $ elemIndex key keys
  where keyVector = partialKeys node
        keysLength = UMV.length keyVector

maybeGetChild :: Node a -> Word8 -> IO (Maybe (Node a))
maybeGetChild node key = do
  ix <- keyIndex node key
  case (ix) of
    Nothing -> return $ Nothing
    Just i -> do
      c <- V.read (pointers node) i
      return $ Just c


addChild :: Node a -> Byte -> Node a -> IO ()
addChild parent key child = do
    -- print $ "keyslength " ++ (show keysLength)
    -- print "reading parent keys"
    parentKeys <- mapM (\i -> UMV.read (partialKeys parent) i) [0..(keysLength - 1)]
    -- print "reading child keys"
    children <- mapM (\i -> V.read (pointers parent) i) [0..(keysLength - 1)]
    let (f, s) = L.splitAt 1 $ ((zip parentKeys children) ++ [(key, child)])
    let pairs = f ++ (sortOn fst $ filter (\(x, _) -> x /= 0) s)
    mapM_ (\i -> do
                    print $ "writing index " ++ (show i) ++ " " ++ (show $ fst $ pairs !! i)
                    UMV.write (partialKeys parent) i (fst $ pairs !! i)
                    V.write (pointers parent) i (snd $ pairs !! i)
          ) [0..(L.length pairs) - 1]
    return ()
  where keysLength = UMV.length (partialKeys parent)

growNode :: Node a -> IO (Node a)
growNode Empty = newNode4
growNode (Leaf k l) = newNode4
growNode (Node4 pk po pl p) = do
  nPk <- UMV.unsafeGrow pk 12
  nP <- V.unsafeGrow po 12
  return $ Node16 nPk nP pl p
growNode (Node16 pk po pl p) = do
  nPk <- UMV.unsafeGrow pk 32
  nP <- V.unsafeGrow po 32
  return $ Node48 nPk nP pl p
growNode (Node48 pk po pl p) = do
  nPk <- UMV.unsafeGrow pk 208
  nP <- V.unsafeGrow po 208
  return $ Node256 nPk nP pl p

shrinkNode :: Node a -> IO (Node a)
shrinkNode Empty = return Empty
shrinkNode (Leaf k l) = return Empty
shrinkNode (Node4 pk po pl p) = do
  val <- V.read po 0 -- Could check if this is definitely a leaf, but it should be in this case
  return $ val
shrinkNode (Node16 pk po pl p) = do
  let nPk = UMV.unsafeTake 4 pk
  let nP = V.unsafeTake 4 po
  return $ Node4 nPk nP pl p
shrinkNode (Node48 pk po pl p) = do
  let nPk = UMV.unsafeTake 16 pk
  let nP = V.unsafeTake 16 po
  return $ Node16 nPk nP pl p
shrinkNode (Node256 pk po pl p) = do
  let nPk = UMV.unsafeTake 48 pk
  let nP = V.unsafeTake 48 po
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
leafMatches :: Node a -> Key -> Int -> Bool
leafMatches (Leaf k _) key _ = if k == key then True else False

-- Not in paper, just did it
testPrefix :: Node a -> Key -> IO Bool
testPrefix node key = if (prefixLen node == 0) then return True else do
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  activePref <- UV.freeze $ UMV.take (fromIntegral $ prefixLen node) (prefix node)
  let activeKey = BS.take prefixLength key
  let matches = foldl (\m (k, kk) -> m && (k == kk)) True (zip (UV.toList activePref) (BS.unpack activeKey))
  print "matches?"
  print matches
  return matches

-- Returns number of matches between node "prefix" and key.
-- I can maybe do this another way, (see testPrefix), but ehh
checkPrefix :: Node a -> Key -> Int -> IO Int
checkPrefix Empty _ _ = return 0
checkPrefix (Leaf leafKey _) key depth = return matches
        -- zipped = zip (BS.unpack $ BS.drop depth leafKey) (BS.unpack $ BS.drop depth key)
        -- matches = foldl (\m (k, kk) -> if k == kk then m + 1 else m) 0 zipped
        where matches = (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) (BS.unpack $ BS.drop depth leafKey) (BS.unpack $ BS.drop depth key)
checkPrefix node k depth = if (prefixLen node == 0) then return 0 else do
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  nodePrefix <- UV.freeze $ UMV.take prefixLength (prefix node)
  let activeKey = BS.take prefixLength $ BS.drop depth k -- Is this right? drop to depth, pick up prefixLen length?
  let matches = (\ l r -> (foldr (\ i r -> if i then 1 + r else 0) 0 $ zipWith (==)  l r)) (UV.toList nodePrefix) (BS.unpack activeKey)
  print $ "checkPrefix matches " ++ (show matches)
  print $ show $ nodePrefix
  return matches

-- INSERT
insert :: Node a -> BS.ByteString -> a -> Int -> IO (Node a)
insert Empty bs val _= do
  return $ Leaf bs val
insert leaf@(Leaf k l) key val depth = do
  print "adding to leaf"
  -- Accumulate prefix for new parent
  pLen <- checkPrefix leaf key depth
  print $ "length shared prefix " ++ (show pLen)
  let sharedPrefix = BS.take pLen $ BS.drop depth key
  sharedPrefixVector <- UV.thaw $ UV.fromList $ BS.unpack $ BS.take maxPrefixSize sharedPrefix
  -- Make new parent
  _newParent <- newNode4

  let newParent = _newParent{prefix = sharedPrefixVector, prefixLen = (fromIntegral pLen)}
  -- let newDepth = depth + pLen
  print "about to add children"
  print $ "adding child " ++ (show (BS.index k (depth + pLen)))
  addChild newParent (BS.index k (depth + pLen)) leaf
  print $ "adding other child " ++ (show $ BS.index key (depth + pLen))
  addChild newParent (BS.index key (depth + pLen)) (Leaf key val)
  -- insert newParent k l newDepth
  -- insert newParent key val newDepth
  print "returning newParent"
  return newParent
insert node key val depth = do
  pLen <- checkPrefix node key depth
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  print "inserting key"
  print $ BS.drop depth key
  case pLen == prefixLength of
    False -> do -- Prefix length mismatch! Split the prefix and add a new node
      print "prefix length mismatch"
      _newNode <- newNode4
      let sharedPrefixVector = UMV.take pLen (prefix node)
      let newNode = _newNode{prefix = sharedPrefixVector, prefixLen = (fromIntegral pLen)}
      addChild newNode (BS.index key (depth + pLen)) (Leaf key val)
      let sharedPrefixVector = UMV.drop pLen (prefix node)
      let newChild = node{ prefixLen = fromIntegral $ (prefixLen node) - (fromIntegral $ pLen + 1), prefix = sharedPrefixVector}
      newKey <- UMV.read (prefix newChild) 0
      addChild newNode newKey newChild
      return newNode
    True -> do -- No length mismatch, this is correct so far
      print "no mismatch"
      let newDepth = fromIntegral $ depth + (fromIntegral $ prefixLen node)
      let keyByte = BS.index key newDepth
      ix <- keyIndex node keyByte
      case (ix) of
        Just i -> do
          childKey <- UMV.read (partialKeys node) i
          child <- fmap fromJust (maybeGetChild node childKey)
          case child of
            Empty -> do -- If child is empty, replace with Leaf
              print "child was empty, adding leaf"
              V.write (pointers node) i (Leaf key val)
              return node
            l@(Leaf kk vv) -> do -- If leaf, insert replacement thing
              print "child was leaf, replacing with node"
              newChild <- insert l key val (depth + 1)
              V.write (pointers node) i newChild
              return node
            _ -> do -- If child is node, check size, prepare to grow it if necessary
              print "child was node, checking size"
              full <- wouldBeFull child keyByte
              case full of
                True -> do
                  print "child was full, expanding child"
                  newChild <- growNode child
                  newChild <- insert newChild key val (depth + 1)
                  V.write (pointers node) i newChild
                  return node
                False -> do
                  print "child not full, inserting into child"
                  insert child key val (depth + 1)
                  return node
        Nothing -> do -- New thing!
          print "was new thing, adding leaf child"
          addChild node (BS.index key newDepth) (Leaf key val)
          return node

-- SEARCH
search :: Node a -> Key -> Int -> IO (Node a)
search Empty _ _ = do
  -- print "encountered empty"
  return Empty
search leaf@(Leaf _ _) key depth = do
  -- print "encountered leaf"
  -- print $ "depth: " ++ (show depth)
  -- print $ "key: " ++ (show key)
  -- SLIGHT MODIFICATION: ALWAYS CHECKS FULL KEY RATHER THAN PREFIX
  return $ if (leafMatches leaf key depth) then leaf else Empty
search node key depth = if key == BS.empty then return Empty else do
  -- Test to make sure key matches any on-node prefix
  -- prefixMatches <- testPrefix node keyRemainder
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  prefixMatches <- (checkPrefix node key depth)
  case prefixMatches == prefixLength of
    False -> return Empty
    True -> do
      let newDepth = depth + (fromIntegral $ prefixLen node)
      next <- maybeGetChild node (BS.index key newDepth)
      case next of
        Nothing -> return Empty
        Just n -> search n key (newDepth + 1)

data DeletionStatus a = NotFound |
                        DeletedLeaf |
                        DeletedChild |
                        ResizedChild { newChild :: Node a } |
                        Complete
-- DELETE
remove :: Node a -> Key -> Int -> IO (DeletionStatus a)
remove Empty _ _ = return NotFound
remove l@(Leaf k _) key _ = if k == key then return DeletedLeaf else return NotFound
remove node key depth = do
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  prefixMatches <- (checkPrefix node key depth)
  case prefixMatches == prefixLength of
    False -> return NotFound
    True -> do
      let newDepth = depth + (fromIntegral $ prefixLen node)
      let thisKey = (BS.index key newDepth)
      next <- maybeGetChild node thisKey
      ix <- keyIndex node thisKey
      case next of
        Nothing -> return NotFound
        Just child -> do
          removeStatus <- remove child key (newDepth + 1)
          let keyIndex = fromJust ix
          case removeStatus of
            NotFound -> return NotFound
            DeletedLeaf -> do
              let keysLength = UMV.length $ partialKeys node
              -- Remove and shift keys
              mapM_ (\i -> if i < keyIndex
                           then return ()
                           else if i == keysLength - 1
                                then UMV.write (partialKeys node) i 0
                                else (UMV.read (partialKeys node) (i + 1)) >>= (\k -> UMV.write (partialKeys node) i k)
                    ) [0..(keysLength - 1)]
              -- Remove and shift pointers
              mapM_ (\i -> if i < keyIndex
                           then return ()
                           else if i == keysLength - 1
                                then V.write (pointers node) i Empty
                                else (V.read (pointers node) (i + 1)) >>= (\k -> V.write (pointers node) i k)
                    ) [0..(keysLength - 1)]
              s <- shouldShrink node
              case s of
                False -> return $ DeletedChild
                True -> do
                  newNode <- shrinkNode node
                  return $ ResizedChild newNode
            ResizedChild newChild -> do
              V.write (pointers node) keyIndex newChild
              return Complete
            Complete -> return Complete
          -- TODO: Case on newChild here to determine what to do
          -- V.write (pointers node) (fromJust ix) newChild
          -- return node
  -- let (h, t) = fromJust $ BS.uncons key
  -- ix <- keyIndex node h
  -- case ix of
  --   Nothing -> return node
  --   Just i -> do
  --     child <- maybeGetChild node h
  --     newChild <- remove (fromJust child) t
  --     V.write (pointers node) i newChild
  --     return node



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
        printVal v = if (V.length v == 0) then return () else do
          val <- V.read v 0
          print "found value:"
          printNode val
          printVal $ V.tail v


main :: IO ()
main = do
  let newART = Empty :: Node String
  print "adding blah"
  newART <- insert newART (encode "0blah") "blah" 0
  print "adding clah"
  newART <- insert newART (encode "0clah") "clah" 0
  print "adding dlah"
  newART <- insert newART (encode "0dlah") "clah" 0
  -- newART <- insert newART (encode "elah") "clah"
  print (encode "blah")
  print "printing node"
  printNode newART
  -- full <- isFull newART
  -- print "full?"
  -- print full
  -- print newART
  -- (isFull Empty) >>= print
  print "searching for blah"
  result <- search newART (encode "0blah") 0
  printNode result
  print "searching for dlah"
  result2 <- search newART (encode "0dlah") 0
  printNode result2
  print "searching for bbbb"
  result2 <- search newART (encode "bbbb") 0
  printNode result2
  remove newART (encode "0blah") 0
  print "searching for blah again"
  result2 <- search newART (encode "0blah") 0
  printNode result2
  return ()
  -- print newART
