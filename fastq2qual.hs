module Main
where

import Bio.Sequence
import System
import System.IO
import Common

convert input output = do
  seqs <- (hReadFastQ input)
  hWriteQual output seqs

main = doFilter convert
