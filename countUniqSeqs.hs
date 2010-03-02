module Main 
where

import Bio.Sequence
import Data.List (sortBy)
import Control.Monad.State
import Control.Monad.ST
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import System.IO
import System (getArgs)
import Common

type SeqMap = M.Map String Integer

-- countSeq :: Sequence a -> StateT SeqMap IO ()
countSeq s = do
       let seq = B8.unpack (seqdata s)
       x <- get
       let count = case M.lookup seq x of
            Just c -> c+1
            Nothing -> 1
                    
       put (M.insert seq count x)

countSeqs :: Handle -> StateT SeqMap IO ()
countSeqs handle = do
  seqs <- liftIO $ hReadFasta handle
  mapM_ countSeq seqs

doCountSeq :: Handle -> Handle -> IO ()
doCountSeq input output = do
  putStrLn "Counting"
  counts <- execStateT (countSeqs input) M.empty

  putStrLn "Sorting"

  let lst = sortBy (applySnd2 compare) (M.toList counts)

  putStrLn "Printing"

  forM_ lst $ \(k,a) -> do
    hPutStr output $ k ++ "\t" ++ (show a) ++ "\n"
                        
main = doFilter doCountSeq
