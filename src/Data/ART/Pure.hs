module Data.ART.Pure where

import Data.ART.Pure.Node

import Data.Maybe
import Data.Word
import Control.Monad.ST
import Data.Primitive.SmallArray
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS

data DeletionStatus = NotFound |
                        DeletedLeaf |
                        DeletedChild |
                        ResizedChild |
                        Complete deriving (Show, Eq)

-- INSERT
insert :: Node a -> BS.ByteString -> a -> Int -> Node a
insert Empty bs val _ = Leaf bs val
insert leaf@(Leaf k l) key val depth = newParent
    where pLen = checkPrefix leaf key depth
          sharedPrefix = BSS.toShort $ BS.take pLen $ BS.drop depth key
          __newParent = newNode4{prefix = sharedPrefix, prefixLen = (fromIntegral pLen)}
          _newParent = addChild __newParent (BS.index k (depth + pLen)) leaf
          newParent = addChild _newParent (BS.index key (depth + pLen)) (Leaf key val)
insert node key val depth = newNode
    where   pLen = checkPrefix node key depth
            prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
            newNode = case pLen == prefixLength of
                False -> do -- Prefix length mismatch.
                    let sharedPrefix = resizePrefix (BSS.toShort $ BS.take pLen $ BSS.fromShort $ prefix node) maxPrefixSize
                    let newNode = newNode4{ prefix = sharedPrefix, prefixLen = (fromIntegral pLen) }
                    let _newNode = addChild newNode (BS.index key (depth + pLen)) (Leaf key val)
                    let unsharedPrefix = resizePrefix (BSS.toShort $ BS.drop pLen $ BSS.fromShort $ prefix node) maxPrefixSize
                    let newChild = node{ prefixLen = fromIntegral $ (prefixLen node) - (fromIntegral $ pLen + 1), prefix = unsharedPrefix}
                    let newKey = BSS.index (prefix newChild) 0
                    addChild _newNode newKey newChild
                True -> do -- No length mismatch
                    let newDepth = fromIntegral $ depth + (fromIntegral $ prefixLen node)
                    let keyByte = BS.index key newDepth
                    let c = maybeGetChild node keyByte
                    case c of
                        Nothing -> addChild node (BS.index key newDepth) (Leaf key val)
                        Just child -> case child of
                            Empty -> addChild node (BS.index key newDepth) (Leaf key val)
                            l@(Leaf kk vv) -> do
                                let newChild = insert l key val (depth + 1)
                                addChild node keyByte newChild
                            _ -> addChild node keyByte (insert child key val (depth + 1))

-- SEARCH
search :: Node a -> BS.ByteString -> Int -> Node a
search Empty _ _ = Empty
-- SLIGHT MODIFICATION: ALWAYS CHECKS FULL KEY RATHER THAN PREFIX
search leaf@(Leaf _ _) key depth = if (leafMatches leaf key depth) then leaf else Empty
search node key depth = if key == BS.empty then Empty else do
  -- Test to make sure key matches any on-node prefix
  let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
  let prefixMatches = checkPrefix node key depth
  case prefixMatches == prefixLength of
    False -> Empty
    True -> do
      let newDepth = depth + (fromIntegral $ prefixLen node)
      case maybeGetChild node (BS.index key newDepth) of
        Nothing -> Empty
        Just n -> search n key (newDepth + 1)

-- DELETE
remove :: Node a -> BS.ByteString -> Int -> (Node a, DeletionStatus)
remove Empty _ _ = (Empty, NotFound)
remove l@(Leaf k _) key _ = if k == key then (Empty, DeletedLeaf) else (Empty, NotFound)
remove node key depth = do
    let prefixLength = min (fromIntegral $ prefixLen node) maxPrefixSize
    let prefixMatches = checkPrefix node key depth
    case prefixMatches == prefixLength of
        False -> (node, NotFound)
        True -> do
            let newDepth = depth + (fromIntegral $ prefixLen node)
            let thisKey = (BS.index key newDepth)
            let next = maybeGetChild node thisKey
            let ix = keyIndex node thisKey
            case next of
                Nothing -> (node, NotFound)
                Just child -> do
                    let (newChild, removeStatus) = remove child key (newDepth + 1)
                    let keyIndex = fromJust ix
                    case removeStatus of
                        NotFound -> (node, NotFound)
                        DeletedChild -> do
                            let updated = addChild (unsetChild node thisKey) thisKey newChild
                            (updated, Complete)
                        DeletedLeaf -> do
                            let updated = unsetChild node thisKey
                            case shouldShrink updated of
                                False -> (updated, DeletedChild)
                                True -> (shrinkNode updated, ResizedChild)
                        ResizedChild -> do
                            let updated = addChild (unsetChild node thisKey) thisKey newChild --TODO: Figure out why this is necessary
                            (updated, Complete)
                        Complete -> (addChild node thisKey newChild, Complete)                   

