import Test.Hspec
import Test.QuickCheck
import Data.ART
import Data.ART.Node

import Data.Word
import Control.Monad
import Data.IORef
import Data.ByteString.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BSC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified Data.List as L
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

keysSorted :: [Word8] -> IO Bool
keysSorted keys = do
    node <- newNode4
    node <- foldM (\n k -> insert n (BSC.pack [k]) k 0) node keys
    if isNode256 node then return True else do
        nodeKeys <- UV.freeze $ partialKeys node
        nodeKeys <- pure $ UV.toList nodeKeys
        nKeys <- (readIORef $ numKeys node) >>= (return . fromIntegral)
        return $ (take nKeys $ nodeKeys) == (L.sort $ take nKeys $ nodeKeys)

main :: IO ()
main = hspec $ do
    describe "Insertion" $ do
        describe "Node key sorting" $ do
            it "should result in sorted vector of keys" $ do
                let kv = map (\i -> (BS.pack $ show i, i)) [1..4]
                node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
                keys <- UV.freeze $ partialKeys node
                keys `shouldBe` UV.fromList ([49, 50, 51, 52] :: [Word8])

            it "should result in mostly sorted vector of keys when partially full" $ do
                let kv = map (\i -> (BS.pack $ show i, i)) [1, 2]
                node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
                keys <- UV.freeze $ partialKeys node
                keys `shouldBe` UV.fromList ([49, 50, 0, 0] :: [Word8])

        describe "Node growth" $ do
            it "should grow to 16 if more than 4 nodes inserted" $ do
                let kv = map (\i -> (BS.pack $ show i, i)) [1..5]
                node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
                (isNode16 node) `shouldBe` True

            it "should grow to 48 if more than 16 nodes inserted" $ do
                let kv = map (\i -> (BS.pack $ show i, i)) ["1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "a", "b", "c", "d", "e", "f", "g"]
                node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
                (isNode48 node) `shouldBe` True

            it "should grow to 256 if more than 48 nodes inserted" $ do
                let kv = map (\i -> (BS.pack $ show i, i)) (['0'..'z'])
                node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
                (isNode256 node) `shouldBe` True

        describe "Node prefixes" $ do
            it "should generate node containing shared prefix" $ do
                let node = Empty
                node <- insert node (BS.pack "abcde1") 0 0
                node <- insert node (BS.pack "abcde2") 1 0
                p <- UV.freeze $ prefix node
                (UV.take (fromIntegral $ prefixLen node) p) `shouldBe` (UV.fromList ([97,98,99,100,101] :: [Word8]))

    describe "Search" $ do
        it "should return empty when searching empty node" $ do
            result <- search Empty (BS.pack "1") 0
            (isEmpty result) `shouldBe` True

        it "should return leaf when searching for existing key" $ do
            let kv = map (\i -> (BS.pack $ show i, i)) [1, 2]
            node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
            result <- search node (BS.pack "1") 0
            let (k, v) = (\(Leaf k v) -> (k, v)) result
            k `shouldBe` (BS.pack "1")
            v `shouldBe` 1

        it "should return leaf when searching for longer key with siblings and prefixes and whatever" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS.pack $ prefix ++ (show i), i)) [1, 2, 3]
            node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
            result <- search node (BS.pack $ prefix ++ "1") 0
            let (k, v) = (\(Leaf k v) -> (k, v)) result
            k `shouldBe` (BS.pack $ prefix ++ "1")
            v `shouldBe` 1

    describe "Remove" $ do
        it "should return NotFound for Empty nodes" $ do
            result <- remove Empty (BS.pack "1") 0
            result `shouldBe` NotFound
        it "should return NotFound if key not found" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS.pack $ prefix ++ (show i), i)) [1, 2, 3]
            node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
            result <- remove node (BS.pack $ "1") 0
            result `shouldBe` NotFound
        it "should return DeletedChild if thing deleted with no resize necessary" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS.pack $ prefix ++ (show i), i)) [1, 2, 3]
            node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
            result <- remove node (BS.pack $ prefix ++ "1") 0
            result `shouldBe` DeletedChild
            result <- search node (BS.pack $ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True
            keys <- UV.freeze $ partialKeys node
            keys `shouldBe` UV.fromList ([50, 51, 0, 0] :: [Word8])
        it "should return ResizedChild if thing deleted with resize necessary" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS.pack $ prefix ++ (show i), i)) [1, 2]
            node <- foldM (\n (k, v) -> insert n k v 0) Empty kv
            keys <- UV.freeze $ partialKeys node
            result <- remove node (BS.pack $ prefix ++ "1") 0
            nk <- readIORef $ numKeys node
            (show result) `shouldBe` "ResizedChild {newChild = Leaf}"
            result <- search node (BS.pack $ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True
        it "should return Complete if thing deleted from child" $ do
            let prefix = "dsklfajldsafkjldfsakjflskjdsalkfjdsfakjl"
            let kv = map (\i -> (BS.pack $ (show i) ++ prefix ++ (show i), i)) [1, 2, 3]
            node <- foldM (\n (k, v) -> insert n k v 0) Empty (kv ++ [(BS.pack $ "1" ++ prefix ++ "q", 0)])
            result <- remove node (BS.pack $ "1" ++ prefix ++ "1") 0
            result `shouldBe` Complete
            result <- search node (BS.pack $ "1" ++ prefix ++ "1") 0
            (isEmpty result) `shouldBe` True
    describe "In practice" $ do
        it "should store and retrieve 17 random keys" $ do
            randomKeys <- mapM random (take 17 $ repeat 10)
            node <- foldM (\n k -> insert n k k 0) Empty randomKeys
            results <- mapM (\k -> search node k 0) randomKeys
            isEmpty node `shouldBe` False
            mapM_ (\k -> do
                n <- search node k 0
                isEmpty n `shouldBe` False
                ((\(Leaf k v) -> k) n) `shouldBe` k
                ) randomKeys
        it "should store and retrieve 49 random keys" $ do
            randomKeys <- mapM random (take 49 $ repeat 10)
            node <- foldM (\n k -> insert n k k 0) Empty randomKeys
            results <- mapM (\k -> search node k 0) randomKeys
            isEmpty node `shouldBe` False
            mapM_ (\k -> do
                n <- search node k 0
                isEmpty n `shouldBe` False
                ((\(Leaf k v) -> k) n) `shouldBe` k
                ) randomKeys
        it "should store and retrieve 1k of random keys" $ do
            randomKeys <- mapM random (take 1000 $ repeat 10)
            node <- foldM (\n k -> insert n k k 0) Empty randomKeys
            results <- mapM (\k -> search node k 0) randomKeys
            isEmpty node `shouldBe` False
            mapM_ (\k -> do
                n <- search node k 0
                isEmpty n `shouldBe` False
                ((\(Leaf k v) -> k) n) `shouldBe` k
                ) randomKeys
    -- -- describe "in practice" $ do
    -- --     it "should insert any arbitrary set of keys in sorted order" $ do
    -- --         property keysSorted
