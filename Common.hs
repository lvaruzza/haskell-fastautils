module Common (
               doFilter,
               applySnd2               
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

--- Apply f x y to snd of x and y
applySnd2 :: (b -> b -> c) -> (a,b) -> (a,b) -> c
applySnd2 f x y = f (snd x) (snd y)

