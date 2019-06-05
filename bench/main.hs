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

setupOtherEnv :: IO ([BS.ByteString], Node BS.ByteString, Node BS.ByteString, Node BS.ByteString, Node BS.ByteString)
setupOtherEnv = do
    randomKeys <- mapM random (take 100 $ repeat 10)
    node4 <- newNode4
    node16 <- newNode16
    node48 <- newNode48
    node256 <- newNode256
    return (randomKeys, node4, node16, node48, node256)

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
    env setupOtherEnv $ \ ~(keys, node4, node16, _, _) -> bgroup "small stuff" [
        bgroup "setChild" [
            -- bench "1 key into Node4" $ whnfIO $ setChild node4 (0 :: Word8) (Leaf (keys !! 0) (keys !! 0))),
            bench "4 keys into Node4" $ whnfIO $ mapM (\(k, v) -> setChild node4 k (Leaf v v) ) (zip [0..3] (take 4 keys))
        ]
    ]
    ]