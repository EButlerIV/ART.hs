import Test.Hspec
import Test.QuickCheck
import Data.ART.Pure
import Data.ART.Pure.Node

import Data.Word
import Control.Monad
import Control.Monad.ST
import Data.Primitive.SmallArray
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS8

prop_revapp :: [Int] -> [Int] -> Bool
prop_revapp xs ys = reverse (xs++ys) == reverse xs ++ reverse ys

alwaysSorted :: [Word8] -> Bool
alwaysSorted keys = if length keys < 4 then True else (filledLength == 16) && (fst sorted)
    where blank = BSS.toShort $ BS.replicate 16 0
          filled = foldl (\ks (k, i) -> fst $ insertKey ks k (if i < 5 then i else 4)) blank (zip keys [0..])
          filledLength = length (BSS.unpack filled)
          sorted = foldl (\(t, a) b -> if t then (a <= b, b) else (False, b)) (True, 0) (BSS.unpack filled)


main :: IO ()
main = hspec $ do
    describe "insertKey" $ do
        it "should insert new key at index 0 if no keys present" $ do
            let keys = BSS.toShort $ BS.replicate 4 0
            let key = (1 :: Word8)
            let (newKeys, newIx) = insertKey keys key 0
            (BSS.unpack newKeys) `shouldBe` ([1, 0, 0, 0] :: [Word8])
            newIx `shouldBe` 0
        it "should insert new key before previous if lower" $ do
            let keys = BSS.toShort $ BS.replicate 4 0
            let key1 = (4 :: Word8)
            let key2 = (1 :: Word8)
            let (newKeys, newIx) = insertKey (fst $ insertKey keys key1 0) key2 1
            (BSS.unpack newKeys) `shouldBe` ([1, 4, 0, 0] :: [Word8])
            newIx `shouldBe` 0
        it "should insert duplicate keys over previous" $ do
            let keys = BSS.pack [0, 1, 0, 0]
            let (newKeys, newIx) = insertKey keys (1 :: Word8) 2
            newKeys `shouldBe` keys
            newIx `shouldBe` 1

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
    describe "Node growth" $ do
        it "should grow to 16 if more than 4 nodes inserted" $ do
            let kv = map (\i -> (BS8.pack $ show i, i)) [1..5]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (isNode16 node) `shouldBe` True

        it "should grow to 48 if more than 16 nodes inserted" $ do
            let kv = map (\i -> (BS8.pack $ show i, i)) ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "a", "b", "c", "d", "e", "f", "g"]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (isNode48 node) `shouldBe` True

        it "should grow to 256 if more than 48 nodes inserted" $ do
            let kv = map (\i -> (BS8.pack $ show i, i)) (['0'..'z'])
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            (isNode256 node) `shouldBe` True

    describe "Node prefixes" $ do
        it "should generate node containing shared prefix" $ do
            let node = Empty
            let _node = insert node (BS8.pack "abcde1") 0 0
            let __node = insert _node (BS8.pack "abcde2") 1 0
            (prefix __node) `shouldBe` (BSS.pack ([97,98,99,100,101] :: [Word8]))
    
    describe "Search" $ do
        it "should return empty when searching empty node" $ do
            let result = search Empty (BS8.pack "1") 0
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

        -- it "should return DeletedChild if thing deleted with no resize necessary" $ do
        --     let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
        --     let kv = map (\i -> (BS8.pack $ prefix ++ (show i), i)) [1, 2, 3]
        --     let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
        --     let (node, result) = remove node (BS8.pack $ prefix ++ "1") 0
        --     result `shouldBe` DeletedChild
        --     let result = search node (BS8.pack $ prefix ++ "1") 0
        --     (isEmpty result) `shouldBe` True
        --     (partialKeys node) `shouldBe` BSS.pack ([50, 51, 0, 0] :: [Word8])

        it "should return ResizedChild if thing deleted with resize necessary" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS8.pack $ prefix ++ (show i), i)) [1, 2]
            let node = foldl (\n (k, v) -> insert n k v 0) Empty kv
            let keys = partialKeys node
            let (node, result) = remove node (BS8.pack $ prefix ++ "1") 0
            result `shouldBe` ResizedChild

            let result = search node (BS8.pack $ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True

        it "should return Complete if thing deleted from child" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS8.pack $ (show i) ++ prefix ++ (show i), i)) [1, 2, 3]
            let _node = foldl (\n (k, v) -> insert n k v 0) Empty (kv ++ [(BS8.pack $ "1" ++ prefix ++ "q", 0)])
            let (node, result) = remove _node (BS8.pack $ "1" ++ prefix ++ "1") 0
            result `shouldBe` Complete
            let result = search node (BS8.pack $ "1" ++ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True