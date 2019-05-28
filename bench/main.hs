{-# LANGUAGE BangPatterns #-}
module Main where

import Data.ART
import qualified Data.Trie as T
import Data.ART.Node
import Data.ByteString.Random
import Control.Monad
import qualified Data.ByteString.Char8 as BS

import Criterion.Main


setupEnv :: IO ([BS.ByteString])
setupEnv = do
    randomKeys <- mapM random (take 100 $ repeat 10)
    -- Don't have a deep comparison instance for tries yet, lol
    -- let fullTrie = foldl (\trie key -> T.insert key key trie) T.empty randomKeys
    return randomKeys


main :: IO ()
main = defaultMain [
    env setupEnv $ \keys -> bgroup "insert" [
                bench "1 keys into ART" $ whnfIO $ insert smallTree (BS.pack "1") (BS.pack "1") 0,
                bench "100 keys length 10 into ART" $ whnfIO $ mapM (\w -> insert bigTree w w 0) keys
                -- bench "1 keys into Trie" $ nf (\k -> foldl (\trie key -> (T.insert key key trie)) T.empty k) keys
            ],
    env setupEnv $ \keys -> bgroup "search" [
                bench "1 keys" $ whnfIO $ search smallTree (BS.pack "1") 0,
                bench "100 keys length 10" $ whnfIO $ (mapM (\w -> search bigTree w 0) keys)
                -- bench "100 keys length 10 from trie" $ nf (\k -> map (\k -> (T.lookup k fullTrie)) k) keys
            ],
    env (mapM random (take 100 $ repeat 10)) $ \ keys -> bgroup "Trie comparison" [
            bench "insert 100 random keys" $ whnf (\k -> foldl (\trie key -> (T.insert key key trie)) T.empty k) keys
        ]
    ]
    where smallTree = Empty
          bigTree = Empty
