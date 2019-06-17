{-# LANGUAGE BangPatterns #-}
module Main where

import Data.ART
import qualified Data.Trie as T
import Data.ART.Node
import Data.ByteString.Random
import Control.Monad
import Control.DeepSeq
import Data.Word
import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as BS0

import Criterion.Main

instance (NFData a) => NFData (T.Trie a) where rnf = rwhnf
instance (NFData a) => NFData (Node a) where rnf = rwhnf

-- setupEnv :: IO ([BS.ByteString], T.Trie BS.ByteString)
setupEnv = do
    randomKeys <- mapM random (take 100 $ repeat 10)
    let fullTrie = foldl (\trie key -> T.insert key key trie) T.empty randomKeys
    let fullMap = foldl (\m key -> Map.insert key key m) Map.empty randomKeys
    fullART <- foldM (\n k -> insert n k k 0) Empty randomKeys
    return (randomKeys, fullTrie, fullMap, fullART)

setupOtherEnv :: IO ([BS.ByteString], [[(Word8, BS.ByteString)]])
setupOtherEnv = do
    randomKeys <- mapM random (take 100 $ repeat 10)
    let kvs = map (\x -> (zip (map fromIntegral [0..x-1]) (take x randomKeys))) ([3, 15, 41, 255])
    return (randomKeys, kvs)

setupFullNodes :: IO (Node BS.ByteString, Node BS.ByteString, Node BS.ByteString, Node BS.ByteString)
setupFullNodes = do
    randomKeys <- mapM random (take 100 $ repeat 10)
    let kvs = map (\x -> (zip (map fromIntegral [0..x-1]) (take x randomKeys))) ([3, 15, 41, 255])
    node4 <- newNode4
    node16 <- newNode16
    node48 <- newNode48
    node256 <- newNode256
    mapM (\(i, n) -> mapM (\(k, v) -> setChild n k (Leaf v v)) (kvs !! i) ) (zip [0..3] [node4, node16, node48, node256])
    return (node4, node16, node48, node256)

setupFullNode :: IO (Node Word8, [Word8])
setupFullNode = do
    randomKey <- random 4
    let randomKeys = (BS0.unpack randomKey)
    node4 <- newNode4
    mapM (\k -> setChild node4 k (Leaf (BS0.pack [k]) k)) randomKeys
    return (node4, randomKeys)

main :: IO ()
main = defaultMain [
    env setupEnv $ \ ~(keys, fullTrie, fullMap, fullART) -> bgroup "big stuff" [
        bgroup "insert" [
            bench "1 keys into ART" $ whnfIO $ insert Empty (keys !! 0) (keys !! 0) 0,
            bench "1 keys into Trie" $ nf (\k -> T.insert k k T.empty) (keys !! 0),
            bench "1 keys into Map" $ nf (\k -> Map.insert k k Map.empty) (keys !! 0),
            bench "100 keys length 10 into ART" $ whnfIO $ mapM (\w -> insert Empty w w 0) keys,
            bench "100 keys length 10 into Trie" $ nf (\k -> foldl (\trie key -> (T.insert key key trie)) T.empty k) keys,
            bench "100 keys length 10 into Map" $ nf (\k -> foldl (\trie key -> (Map.insert key key trie)) Map.empty k) keys
        ],
        bgroup "search" [
            bench "1 keys from ART" $ whnfIO $ search fullART (keys !! 0) 0,
            bench "1 keys from Trie" $ nf (\k -> T.lookup k fullTrie) (keys !! 0),
            bench "1 keys from Map" $ nf (\k -> Map.lookup k fullMap) (keys !! 0),
            bench "100 keys length 10 from ART" $ whnfIO $ (mapM (\w -> search fullART w 0) keys),
            bench "100 keys length 10 from Trie" $ nf (\k -> map (\k -> (T.lookup k fullTrie)) k) keys,
            bench "100 keys length 10 from Map" $ nf (\k -> map (\k -> (Map.lookup k fullMap)) k) keys
        ],
        bgroup "remove" [
            bench "1 keys from ART" $ whnfIO $ remove fullART (keys !! 0) 0,
            bench "1 keys from Trie" $ nf (\k -> T.delete k fullTrie) (keys !! 0),
            bench "1 keys from Map" $ nf (\k -> Map.delete k fullMap) (keys !! 0),
            bench "100 keys length 10 from ART" $ whnfIO $ (mapM (\w -> remove fullART w 0) keys),
            bench "100 keys length 10 from Trie" $ nf (\k -> map (\k -> (T.delete k fullTrie)) k) keys,
            bench "100 keys length 10 from Map" $ nf (\k -> map (\k -> (Map.delete k fullMap)) k) keys
        ]
    ],
    env setupOtherEnv $ \ ~(keys, kvs) -> bgroup "small stuff" [
        bgroup "setChild" [
            bench "4 keys into Node4" $ perRunEnv newNode4 $ \n -> mapM (\(k, v) -> setChild n k (Leaf v v) ) (kvs !! 0),
            bench "16 keys into Node16" $ perRunEnv newNode16 $ \n -> (\kv -> mapM (\(k, v) -> setChild n k (Leaf v v)) kv) (kvs !! 1),
            bench "48 keys into Node48" $ perRunEnv newNode48 $ \n -> (\kv -> (mapM (\(k, v) -> setChild n k (Leaf v v)) kv)) (kvs !! 2),
            bench "256 keys into Node256" $ perRunEnv newNode256 $ \n -> (\kv -> (mapM (\(k, v) -> setChild n k (Leaf v v)) kv)) (kvs !! 3)
        ],
        bgroup "unsetChild" [
            bench "4 keys from Node4" $ perRunEnv setupFullNode $ \(n, keys) -> mapM (\k -> unsetChild n k) keys,
            bench "4 keys from Node4 no copy" $ perRunEnv setupFullNode $ \(n, keys) -> mapM (\k -> unsetChildNoCopy n k) keys
        ],
        bgroup "keyIndex" [
            bench "use keyIndex to test node4 for key membership" $ perRunEnv setupFullNode $ \(n, keys) -> mapM (\k -> keyIndex n k) keys,
            bench "use getIx to test node4 for key membership" $ perRunEnv setupFullNode $ \(n, keys) -> mapM (\k -> getIx (partialKeys n) k) keys
        ]
    ]
    ]