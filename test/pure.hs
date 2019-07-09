import Test.Hspec
import Test.QuickCheck
import Data.ART.Pure
import Data.ART.Pure.Node

import Data.Word
import Control.Monad
import Control.Monad.ST
import Data.Primitive.SmallArray
import Data.ByteString.Random
import qualified Data.List as L
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS8

keysSorted :: [Word8] -> Bool
keysSorted keys = if isNode256 node then True else (take (fromIntegral $ numKeys node) $ BSS.unpack $ partialKeys node) == (L.sort $ take (fromIntegral $ numKeys node) $ BSS.unpack $ partialKeys node)
    where node = foldl (\n k -> insert n (BS.pack [k]) k 0) newNode4 keys 

-- childrenSorted :: [Word8] -> Bool
-- childrenSorted keys = if length keys > 0 then all (\(k, Just (Leaf _ v)) -> v == k) mChildren else True
--     where node = foldl (\n k -> insert n (BS.pack [k, 0, 0, 0, 0]) k 0) newNode4 keys
--           mChildren = map (\k -> (k, maybeGetChild node k)) keys

main :: IO ()
main = hspec $ do
    describe "insertKey" $ do
        it "should insert new key at index 0 if no keys present" $ do
            let keys = BSS.toShort $ BS.replicate 4 0
            let key = (1 :: Word8)
            let (newKeys, newIx, _) = insertKey keys key 0
            (BSS.unpack newKeys) `shouldBe` ([1, 0, 0, 0] :: [Word8])
            newIx `shouldBe` 0
        it "should insert new key before previous if lower" $ do
            let keys = BSS.toShort $ BS.replicate 4 0
            let key1 = (4 :: Word8)
            let key2 = (1 :: Word8)
            let (newKeys, newIx, _) = insertKey ((\(n, _, _) -> n) $ insertKey keys key1 0) key2 1
            (BSS.unpack newKeys) `shouldBe` ([1, 4, 0, 0] :: [Word8])
            newIx `shouldBe` 0
        it "should insert duplicate keys over previous" $ do
            let keys = BSS.pack [0, 1, 0, 0]
            let (newKeys, newIx, _) = insertKey keys (1 :: Word8) 2
            newKeys `shouldBe` keys
            newIx `shouldBe` 1
        it "should insert four keys in sequence" $ do
            let keys = BSS.pack [0, 0, 0, 0]
            (newKeys, newIx, _) <- pure $ insertKey keys 0 0
            newIx `shouldBe` 0
            newKeys `shouldBe` BSS.pack [0,0,0,0]
            (newKeys, newIx, _) <- pure $ insertKey newKeys 1 1
            newIx `shouldBe` 1
            newKeys `shouldBe` BSS.pack [0,1,0,0]
            (newKeys, newIx, _) <- pure $ insertKey newKeys 2 2
            newIx `shouldBe` 2
            newKeys `shouldBe` BSS.pack [0,1,2,0]
            (newKeys, newIx, _) <- pure $ insertKey newKeys 3 3
            newIx `shouldBe` 3
            newKeys `shouldBe` BSS.pack [0,1,2,3]
        it "should insert four more keys" $ do
            let keys = BSS.pack [0, 1, 2, 3]
            (newKeys, newIx, _) <- pure $ insertKey keys 0 4
            newIx `shouldBe` 0
            newKeys `shouldBe` BSS.pack [0, 1, 2, 3]
            (newKeys, newIx, _) <- pure $ insertKey newKeys 1 4
            newIx `shouldBe` 1
            newKeys `shouldBe` BSS.pack [0, 1, 2, 3]
            (newKeys, newIx, _) <- pure $ insertKey newKeys 2 4
            newIx `shouldBe` 2
            newKeys `shouldBe` BSS.pack [0, 1, 2, 3]
            (newKeys, newIx, _) <- pure $ insertKey newKeys 3 4
            newIx `shouldBe` 3
            newKeys `shouldBe` BSS.pack [0, 1, 2, 3]
        it "should insert any arbitrary set of keys in sorted order" $ do
            property keysSorted

    describe "insertChildAt" $ do
        it "should insert a new child at index" $ do
            let children = smallArrayFromList $ replicate 4 Empty
            let child = (newNode4 :: Node Word8)
            let newChildren = insertChildAt children 0 child
            newChildren `shouldBe` smallArrayFromList [child, Empty, Empty, Empty]
        it "should insert a new child at last index" $ do
            let children = smallArrayFromList $ replicate 4 Empty
            let child = (newNode4 :: Node Word8)
            let newChildren = insertChildAt children 3 child
            newChildren `shouldBe` smallArrayFromList [Empty, Empty, Empty, child]
        it "should insert a new child at index, shifting previous child" $ do
            let children = smallArrayFromList $ replicate 4 Empty
            let child1 = (newNode4 :: Node Word8)
            let child2 = (newNode48 :: Node Word8)
            let _newChildren = insertChildAt children 0 child1
            let newChildren = insertChildAt _newChildren 0 child2
            newChildren `shouldBe` smallArrayFromList [child2, child1, Empty, Empty]

    describe "Insert" $ do
        it "should result in sorted vector of keys" $ do
            let kv = map (\i -> (BS8.pack $ show i, i)) [1..4]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (partialKeys node) `shouldBe` BSS.pack ([49, 50, 51, 52] :: [Word8])
        it "should result in mostly sorted array of keys when partially full" $ do
            let kv = map (\i -> (BS8.pack $ show i, i)) [1, 2]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (partialKeys node) `shouldBe` BSS.pack ([49, 50, 0, 0] :: [Word8])
        -- it "should result in matching inserts on any input" $ do
        --     quickCheck childrenSorted

    describe "Node growth" $ do
        it "should grow to 16 if more than 4 nodes inserted" $ do
            let kv = map (\i -> (BS.pack [i], i)) [1..5]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (isNode16 node) `shouldBe` True
            let keys = map (\(k, _) -> BS.index k 0) kv
            (numKeys node) `shouldBe` 5
            (take 5 $ BSS.unpack $ partialKeys node) `shouldBe` keys
            let leaves = map (\i -> indexSmallArray (pointers node) i) [0..15]
            mapM_ (\i -> ((\(Leaf _ v) -> v) (leaves !! i)) `shouldBe` (keys !! i)) [0..4]

        it "should grow to 48 if more than 16 nodes inserted" $ do
            let kv = map (\i -> (BS.pack [i], i)) [1..17]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (isNode48 node) `shouldBe` True
            let keys = map (\(k, _) -> BS.index k 0) kv
            (numKeys node) `shouldBe` 17
            (take 17 $ BSS.unpack $ partialKeys node) `shouldBe` keys
            let leaves = map (\i -> indexSmallArray (pointers node) (fromIntegral $ BSS.index (partialKeys node) i)) [0..16]
            mapM_ (\i -> ((\(Leaf _ v) -> v) (leaves !! i)) `shouldBe` (keys !! i)) [0..4]


        it "should grow to 256 if more than 48 nodes inserted" $ do
            let kv = map (\i -> (BS8.pack $ show i, i)) (['0'..'z'])
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (numKeys node) `shouldBe` 75
            (isNode256 node) `shouldBe` True

    describe "Node prefixes" $ do
        it "should generate node containing shared prefix" $ do
            let node = Empty
            let _node = insert node (BS8.pack "abcde1") 0 0
            let __node = insert _node (BS8.pack "abcde2") 1 0
            (take 5 $ BSS.unpack $ prefix __node) `shouldBe` ([97,98,99,100,101] :: [Word8])
    
    describe "Search" $ do
        it "should return empty when searching empty node" $ do
            let result = (search Empty (BS8.pack "1") 0 :: Node Int)
            (isEmpty result) `shouldBe` True

        it "should return leaf when searching for existing key" $ do
            let kv = map (\i -> (BS8.pack $ show i, i)) [1, 2]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            let result = search node (BS8.pack "1") 0
            let (k, v) = (\(Leaf k v) -> (k, v)) result
            k `shouldBe` (BS8.pack "1")
            v `shouldBe` 1

        it "should return leaf when searching for longer key with siblings and prefixes and whatever" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS8.pack $ prefix ++ (show i), i)) [1, 2, 3]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            let result = search node (BS8.pack $ prefix ++ "1") 0
            let (k, v) = (\(Leaf k v) -> (k, v)) result
            k `shouldBe` (BS8.pack $ prefix ++ "1")
            v `shouldBe` 1

    describe "Remove" $ do
        it "should return NotFound for Empty nodes" $ do
            let (node, result) = remove Empty (BS8.pack "1") 0
            result `shouldBe` NotFound
            node `shouldBe` (Empty :: Node Word8)

        it "should return NotFound if key not found" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS8.pack $ prefix ++ (show i), i)) [1, 2, 3]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            let (newNode, result) = remove node (BS8.pack $ "1") 0
            result `shouldBe` NotFound

        it "should return DeletedChild if thing deleted with no resize necessary" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS8.pack $ prefix ++ (show i), i)) [1, 2, 3]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (node, result) <- pure $ remove node (BS8.pack $ prefix ++ "1") 0
            result `shouldBe` DeletedChild
            let result = search node (BS8.pack $ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True
            (partialKeys node) `shouldBe` BSS.pack ([50, 51, 0, 0] :: [Word8])

        it "should return ResizedChild if thing deleted with resize necessary" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS8.pack $ prefix ++ (show i), i)) [1, 2]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            let keys = partialKeys node
            (node, result) <- pure $ remove node (BS8.pack $ prefix ++ "1") 0
            result `shouldBe` ResizedChild

            let result = search node (BS8.pack $ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True

        it "should return Complete if thing deleted from child" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS8.pack $ (show i) ++ prefix ++ (show i), i)) [1, 2, 3]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty (kv ++ [(BS8.pack $ "1" ++ prefix ++ "q", 0)])
            (node, result) <- pure $ remove node (BS8.pack $ "1" ++ prefix ++ "1") 0
            result `shouldBe` Complete
            let result = search node (BS8.pack $ "1" ++ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True

    describe "In practice" $ do
        it "should store and retrieve 17 random keys" $ do
            randomKeys <- mapM random (take 17 $ repeat 10)
            node <- pure $ foldl (\n k -> insert n k k 0) Empty randomKeys
            results <- pure $ map (\k -> search node k 0) randomKeys
            isEmpty node `shouldBe` False
            mapM_ (\k -> do
                n <- pure $ search node k 0
                n `shouldBe` Leaf k k
                ) randomKeys
        it "should store and retrieve 1k of random keys" $ do
            randomKeys <- mapM random (take 1000 $ repeat 10)
            node <- pure $ foldl (\n k -> insert n k k 0) Empty randomKeys
            let results = map (\k -> search node k 0) randomKeys
            isEmpty node `shouldBe` False
            mapM_ (\k -> do
                let n = search node k 0
                n `shouldBe` Leaf k k
                ) randomKeys
