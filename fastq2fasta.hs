module Main
where

import Bio.Sequence
import System

main = do
  [nStr,inputFile,outputCSFasta,outputQual] <- getArgs
  seqs <- (readFastQ inputFile)
  writeFasta outputCSFasta (take (read nStr) seqs)
  writeQual outputQual (take (read nStr) seqs)

