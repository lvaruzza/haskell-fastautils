module Main 
where

import Bio.Sequence
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import System.IO
import System (getArgs)
import Common
import qualified Data.HashTable as H
import Data.Int
import Control.Monad

----------------------------------------

type SeqMap = H.HashTable String Int

-- countSeq :: Sequence a -> StateT SeqMap IO ()
countSeq x s = do
       let seq = B8.unpack (seqdata s)
       result <- H.lookup x seq
       case result of
            Just c -> do
                     H.update x seq (c+1)
                     return ()
            Nothing -> do
                     H.insert x seq 1 
                     return ()

-- countSeqs :: Handle -> StateT SeqMap IO ()
countSeqs handle = do
  x <-  (H.new (==) H.hashString)
  seqs <- hReadFasta handle
  mapM_ (countSeq x) seqs
  return x

doCountSeq :: Handle -> Handle -> IO ()
doCountSeq input output = do
     putStrLn "Counting"
     counts <- (countSeqs input)
     lst1 <- H.toList counts
     putStrLn "Printing"
     let lst = lst1 `seq` sortBy (applySnd2 compare) lst1
     forM_ lst $ \(k,a) -> do
         hPutStr output $ k ++ "\t" ++ (show a) ++ "\n"
                        
main = doFilter doCountSeq
