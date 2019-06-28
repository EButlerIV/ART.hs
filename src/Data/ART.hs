module Data.ART where

import Data.ART.Node

import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as UMV

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

data DeletionStatus a = NotFound |
                        DeletedLeaf |
                        DeletedChild |
                        ResizedChild { newChild :: Node a } |
                        Complete deriving (Show)

instance Eq (DeletionStatus a) where
    NotFound == NotFound = True
    DeletedChild == DeletedChild = True
    DeletedLeaf == DeletedLeaf = True
    Complete == Complete = True
    _ == _ = False -- Could do something with ResizedChild, but IDK

-- INSERT
insert :: Node a -> BS.ByteString -> a -> Int -> IO (Node a)
insert Empty bs val _= do
  return $ Leaf bs val
insert leaf@(Leaf k l) key val depth = do
  -- Accumulate prefix for new parent
  pLen <- checkPrefix leaf key depth
  let sharedPrefix = BS.take pLen $ BS.drop depth key
  sharedPrefixVector <- UV.thaw $ UV.fromList $ BS.unpack $ BS.take maxPrefixSize sharedPrefix
  -- Make new parent
  _newParent <- newNode4
  let newParent = _newParent{prefix = sharedPrefixVector, prefixLen = (fromIntegral pLen)}
  setChild newParent (BS.index k (depth + pLen)) leaf
  -- _newParent <- superAddChild newParent (BS.index k (depth + pLen)) leaf
  __newParent <- superAddChild newParent (BS.index key (depth + pLen)) (Leaf key val)
  return __newParent
insert node key val depth = do
  pLen <- checkPrefix node key depth
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  case pLen == prefixLength of
    False -> do -- Prefix length mismatch! Split the prefix and add a new node
      _newNode <- newNode4
      let _sharedPrefixVector = UMV.take pLen (prefix node)
      sharedPrefixVector <- UMV.grow _sharedPrefixVector (maxPrefixSize - (UMV.length _sharedPrefixVector))
      let __newNode = _newNode{prefix = sharedPrefixVector, prefixLen = (fromIntegral pLen)}
      -- addChild newNode (BS.index key (depth + pLen)) (Leaf key val)
      newNode <- superAddChild __newNode (BS.index key (depth + pLen)) (Leaf key val)
      let _sharedPrefixVector = UMV.drop pLen (prefix node)
      sharedPrefixVector <- UMV.grow _sharedPrefixVector (maxPrefixSize - (UMV.length _sharedPrefixVector))
      let newChild = node{ prefixLen = fromIntegral $ (prefixLen node) - (fromIntegral $ pLen + 1), prefix = sharedPrefixVector}
      newKey <- UMV.read (prefix newChild) 0
      -- addChild newNode newKey newChild
      finalNode <- superAddChild newNode newKey newChild
      return finalNode
    True -> do -- No length mismatch, this is correct so far
      let newDepth = fromIntegral $ depth + (fromIntegral $ prefixLen node)
      let keyByte = BS.index key newDepth
      ix <- keyIndex node keyByte
      case (ix) of
        Just i -> do
          childKey <- getKey node i
          child <- fmap fromJust (maybeGetChild node childKey)
          case child of
            Empty -> do -- If child is empty, replace with Leaf
              MV.write (pointers node) i (Leaf key val)
              return node
            l@(Leaf kk vv) -> do -- If leaf, insert replacement thing
              newChild <- insert l key val (depth + 1)
              MV.write (pointers node) i newChild
              return node
            _ -> do -- If child is node, check size, prepare to grow it if necessary -- actually, do this elsewhere
                newestChild <- insert child key val (depth + 1)
                MV.write (pointers node) i newestChild
                return node
        Nothing -> do -- New thing!
          superAddChild node (BS.index key newDepth) (Leaf key val)

-- SEARCH
search :: Node a -> BS.ByteString -> Int -> IO (Node a)
search Empty _ _ = do
  return Empty
search leaf@(Leaf _ _) key depth = do
  -- SLIGHT MODIFICATION: ALWAYS CHECKS FULL KEY RATHER THAN PREFIX
  return $ if (leafMatches leaf key depth) then leaf else Empty
search node key depth = if key == BS.empty then return Empty else do
  -- Test to make sure key matches any on-node prefix
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

-- DELETE
remove :: Node a -> BS.ByteString -> Int -> IO (DeletionStatus a)
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
            DeletedChild -> return Complete
            DeletedLeaf -> do
              unsetChild node thisKey
              s <- shouldShrink node
              case s of
                False -> return $ DeletedChild
                True -> do
                  newNode <- shrinkNode node
                  return $ ResizedChild newNode
            ResizedChild newChild -> do
              MV.write (pointers node) keyIndex newChild
              return Complete
            Complete -> return Complete

-- demo :: IO ()
-- demo = do
--   let newART = Empty :: Node String
--   print "adding blah"
--   newART <- insert newART (BS.pack "0blah") "blah" 0
--   print "adding clah"
--   newART <- insert newART (BS.pack "0clah") "clah" 0
--   print "adding dlah"
--   newART <- insert newART (encode "0dlah") "dlah" 0
--   print "printing node"
--   printNode newART
--   print "searching for blah"
--   result <- search newART (encode "0blah") 0
--   printNode result
--   print "searching for dlah"
--   result2 <- search newART (encode "0dlah") 0
--   printNode result2
--   print "searching for bbbb"
--   result2 <- search newART (encode "bbbb") 0
--   printNode result2
--   remove newART (encode "0blah") 0
--   print "searching for blah again"
--   result2 <- search newART (encode "0blah") 0
--   printNode result2
--   return ()