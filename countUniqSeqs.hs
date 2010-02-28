module Main 
where

import Bio.Sequence
import Data.List (sortBy)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB
import System.IO
import System.Environment (getArgs)

type SeqMap = M.Map SeqData Integer

countSeq :: Sequence a -> StateT SeqMap IO ()
countSeq s = do
       let seq = (seqdata s)
       x <- get
       let count = case M.lookup seq x of
            Just c -> c+1
            Nothing -> 1
                    
       put (M.insert seq count x)

countSeqs :: FilePath -> StateT SeqMap IO ()
countSeqs path = do
  seqs <- liftIO $ readFasta path
  mapM_ countSeq seqs

countSeqsFile :: FilePath -> IO ()
countSeqsFile path = do
     counts <- execStateT (countSeqs path) M.empty

     let lst = sortBy sortFun (M.toList counts)
                      where sortFun (_,a1) (_,a2) = compare a2 a1 

     putStr $ foldl printEntry "" lst
                    where
                        printEntry buff (k, a) = buff ++ (BB.unpack k) ++ "\t" ++ 
                                                       (show a) ++ "\n"
                        
main = do
     args <- getArgs
     case args of
          [path] -> countSeqsFile path
          otherwise -> putStrLn "use ./CountFasta filename"
