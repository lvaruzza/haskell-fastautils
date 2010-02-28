module Main
where

import Bio.Sequence
import System

main = do
  [inputFile,outputQual] <- getArgs
  seqs <- (readFastQ inputFile)
  writeQual outputQual seqs

