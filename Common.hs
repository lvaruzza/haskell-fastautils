module Common (
               doFilter
) where 

import System.IO
import System

doFilter conversor = do
  args <- getArgs
  case args of
    [inputFile,outputQual] -> do
              input <- (openFile inputFile ReadMode) 
              output <- (openFile outputQual WriteMode)
              conversor input output
              hClose input
              hClose output
    [inputFile] -> do
              input <- (openFile inputFile ReadMode) 
              conversor input stdout
    otherwise -> conversor stdin stdout
