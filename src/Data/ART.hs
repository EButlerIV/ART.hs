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
insert Empty bs val _ = return $ Leaf bs val
insert leaf@(Leaf k l) key val depth = case k == key of
    True -> return $ Leaf k val
    False -> do
      -- Accumulate prefix for new parent
      pLen <- checkPrefix leaf key depth
      let sharedPrefix = BS.take pLen $ BS.drop depth key
      sharedPrefixVector <- UV.thaw $ UV.fromList $ BS.unpack $ BS.take maxPrefixSize sharedPrefix
      sharedPrefixVector <- resizePrefix sharedPrefixVector maxPrefixSize
      -- Make new parent
      newParent <- newNode4
      newParent <- pure $ newParent{prefix = sharedPrefixVector, prefixLen = (fromIntegral pLen)}
      setChild newParent (BS.index k (depth + pLen)) leaf
      setChild newParent (BS.index key (depth + pLen)) (Leaf key val)
      return newParent
insert node key val depth = do
  pLen <- checkPrefix node key depth
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  case pLen == prefixLength of
    False -> do -- Prefix length mismatch! Split the prefix and add a new node
      newNode <- newNode4
      sharedPrefixVector <- pure $ UMV.take pLen (prefix node)
      sharedPrefixVector <- resizePrefix sharedPrefixVector maxPrefixSize
      newNode <- pure $ newNode{prefix = sharedPrefixVector, prefixLen = (fromIntegral pLen)}
      -- addChild newNode (BS.index key (depth + pLen)) (Leaf key val)
      setChild newNode (BS.index key (depth + pLen)) (Leaf key val)
      unsharedPrefixVector <- pure $ UMV.drop pLen (prefix node)
      unsharedPrefixVector <- resizePrefix unsharedPrefixVector maxPrefixSize
      let newChild = node{ prefixLen = fromIntegral $ (prefixLen node) - (fromIntegral $ pLen + 1), prefix = unsharedPrefixVector}
      newKey <- UMV.read (prefix newChild) 0
      setChild newNode newKey newChild
      return newNode
    True -> do -- No length mismatch, this is correct so far
      let newDepth = fromIntegral $ depth + (fromIntegral $ prefixLen node)
      let keyByte = BS.index key newDepth
      c <- maybeGetChild node keyByte
      case c of
          Nothing -> do
            superAddChild node keyByte (Leaf key val)
          Just child -> case child of
            Empty -> superAddChild node keyByte (Leaf key val)
            l@(Leaf kk vv) -> do
              newChild <- insert l key val (depth + 1)
              superAddChild node keyByte newChild
            _ -> do
              newChild <- insert child key val (depth + 1)
              superAddChild node keyByte newChild

-- SEARCH
search :: Node a -> BS.ByteString -> Int -> IO (Node a)
search Empty _ d = return Empty
search leaf@(Leaf k v) key depth = case leafMatches leaf key depth of
  True -> return leaf
  False -> return Empty
  -- SLIGHT MODIFICATION: ALWAYS CHECKS FULL KEY RATHER THAN PREFIX
  -- return $ if (leafMatches leaf key depth) then leaf else Empty
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