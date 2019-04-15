{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}

module Main where

import Data.Struct.Internal
import Data.Struct.TH
import Data.Struct
import Control.Exception
import Control.Monad
import Data.Word
import Data.List as L hiding (insert)
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.Array.Unboxed as UA
import Data.Vector.Mutable as V
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.Bits (unsafeShiftR)
import Data.Serialize
import qualified Data.ByteString.Char8 as C


type Key = BS.ByteString
type Byte = Word8

-- There are four kinds of nodes
-- Partialkeys should be unboxed vector
data Node a = Node4 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a))} |
              Node16 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a))} |
              Node48 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a))} |
              Node256 { partialKeys :: (UMV.IOVector Byte), pointers :: (IOVector (Node a))} |
              Leaf Key a | -- Could/should store key prefix in leaf
              Empty

instance (Show a) => Show (Node a) where
  show Empty = "Empty"
  show (Leaf k v) = "Leaf " ++ (show k) ++ " " ++ (show v)
  show _ = "other"

newNode4 :: IO (Node a)
newNode4 = do
  k <- UMV.new 4
  v <- V.replicate 4 Empty
  return $ Node4 k v

newNode16 :: IO (Node a)
newNode16 = do
  k <- UMV.new 16
  v <- V.replicate 16 Empty
  return $ Node16 k v

newNode48 :: IO (Node a)
newNode48 = do
  k <- UMV.new 48
  v <- V.replicate 4 Empty
  return $ Node48 k v

newNode256 :: IO (Node a)
newNode256 = do
  k <- UMV.new 256
  v <- V.replicate 4 Empty
  return $ Node256 k v

searchZero :: UMV.IOVector Word8 -> IO Bool
searchZero empty = return False
searchZero v = do
  val <- UMV.read v 0
  if val == 0 then return True else searchZero (UMV.tail v)

-- Oh, duh. Just check if the last entry is a zero or not
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
    parentKeys <- mapM (\i -> UMV.read (partialKeys parent) i) [0..(keysLength - 1)]
    children <- mapM (\i -> V.read (pointers parent) i) [0..(keysLength - 1)]
    let (f, s) = L.splitAt 1 $ ((zip parentKeys children) ++ [(key, child)])
    let pairs = f ++ (sortOn fst $ filter (\(x, _) -> x /= 0) s)
    mapM_ (\i -> do
                    UMV.write (partialKeys parent) i (fst $ pairs !! i)
                    V.write (pointers parent) i (snd $ pairs !! i)
          ) [0..(L.length pairs) - 1]
    return ()
  where keysLength = UMV.length (partialKeys parent)

growNode :: Node a -> IO (Node a)
growNode Empty = newNode4 -- should probably be undefined or something
growNode (Leaf k l) = newNode4
growNode (Node4 pk p) = do
  nPk <- UMV.unsafeGrow pk 12
  nP <- V.unsafeGrow p 12
  return $ Node16 nPk nP
growNode (Node16 pk p) = do
  nPk <- UMV.unsafeGrow pk 32
  nP <- V.unsafeGrow p 32
  return $ Node48 nPk nP
growNode (Node48 pk p) = do
  nPk <- UMV.unsafeGrow pk 208
  nP <- V.unsafeGrow p 208
  return $ Node256 nPk nP

-- Indicate if node would be full if we attempted to insert a thing
-- Must be both full and not contain the key
wouldBeFull :: Node a -> Word8 -> IO Bool
wouldBeFull Empty _ = return False
wouldBeFull (Leaf _ _ ) _ = return True
wouldBeFull n k = do
  full <- isFull n
  containsKey <- (keyIndex n k) >>= (\x -> return $ x /= Nothing)
  return $ full && containsKey

leafMatches :: Node a -> Key -> Int -> Bool -- TODO: Does depth matter?
leafMatches (Leaf k _) key _ = if k == key then True else False

-- INSERT
insert :: Node a -> BS.ByteString -> a -> IO (Node a)
insert Empty bs val = do
  -- print $ "replacing empty with leaf: " ++ (show bs)
  return $ Leaf bs val
insert (Leaf k l) bs val = do
  -- print "splitting leaf into nodes"
  newParent <- growNode (Leaf k l)
  insert newParent k l
  insert newParent bs val
  return newParent
insert node bs val = do -- this is the one that could be for nodes of any size. Prepare to check child to see if it needs to be expanded
  -- print "preparing to insert into node"
  let (h, t) = fromJust $ BS.uncons bs
  -- print $ "key: " ++ (show h)
  ix <- keyIndex node h
  case (ix) of
    Just i -> do
      -- print "was known thing"
      childKey <- UMV.read (partialKeys node) i
      child <- fmap fromJust (maybeGetChild node childKey)
      case child of
        Empty -> do -- If child is empty, replace with Leaf
          -- print "child was empty, adding leaf"
          V.write (pointers node) i (Leaf t val)
          return node
        l@(Leaf kk vv) -> do -- If leaf, insert replacement thing
          -- print "child was leaf, replacing with node"
          newChild <- insert l t val
          V.write (pointers node) i newChild
          return node
        _ -> do -- If child is node, check size, prepare to grow it if necessary
          -- print "child was node, checking size"
          full <- wouldBeFull child h
          case full of
            True -> do
              -- print "child was full, expanding child"
              newChild <- growNode child
              newChild <- insert newChild t val
              V.write (pointers node) i newChild
              return node
            False -> do
              -- print "child not full, inserting into child"
              insert child t val
              return node
    Nothing -> do -- New thing!
      -- print "was new thing, adding leaf child"
      addChild node h (Leaf t val)
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
  return $ if (leafMatches leaf key depth) then leaf else Empty
search node key depth = if key == BS.empty then return Empty else do
  -- print "encountered node"
  let (h, t) = fromJust $ BS.uncons key
  child <- maybeGetChild node h
  case (child) of
    Nothing -> do
      -- print "got no child for"
      -- print h
      return Empty
    Just n -> do
      -- print $ "did get child for key: " ++ (show h)
      search n t (depth + 1)

-- DELETE
remove :: Node a -> Key -> IO (Node a)
remove Empty  _= return Empty
remove l@(Leaf k _) key = if k == key then return Empty else return l
remove node key = do
  let (h, t) = fromJust $ BS.uncons key
  ix <- keyIndex node h
  case ix of
    Nothing -> return node
    Just i -> do
      child <- maybeGetChild node h
      newChild <- remove (fromJust child) t
      V.write (pointers node) i newChild
      return node



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
  newART <- insert newART (encode "blah") "blah"
  print "adding clah"
  newART <- insert newART (encode "clah") "clah"
  print "adding dlah"
  newART <- insert newART (encode "dlah") "clah"
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
  result <- search newART (encode "blah") 0
  print result
  print "searching for dlah"
  result2 <- search newART (encode "dlah") 0
  print result2
  remove newART (encode "blah")
  print "searching for blah again"
  result2 <- search newART (encode "blah") 0
  print result2
  return ()
  -- print newART
