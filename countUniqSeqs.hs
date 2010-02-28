module Main 
where

import Bio.Sequence
import Data.List (sortBy)
import Control.Monad.State
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as BB
import System.IO
import System (getArgs)
import Common

type SeqMap = M.Map SeqData Integer

countSeq :: Sequence a -> StateT SeqMap IO ()
countSeq s = do
       let seq = (seqdata s)
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
     counts <- execStateT (countSeqs input) M.empty

     let lst = sortBy (applySnd2 compare) (M.toList counts)

     hPutStr output $ foldl printEntry "" lst
         where
           printEntry buff (k, a) = buff ++ (BB.unpack k) ++ "\t" ++ 
                                    (show a) ++ "\n"
                        
main = doFilter doCountSeq
