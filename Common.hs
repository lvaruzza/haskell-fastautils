module Common (
               doFilter,
               applySnd2               
) where 

import System.IO
import System
import Control.Exception (bracket)

-- Give two file handlers to the conversor function. The handlers are decided according to
-- the cmd line arguments, the options are:
--   [inputFile,outputFile] 
--   [inputFile,stdout]
--   [stdin,stdout]
doFilter conversor = do
  args <- getArgs
  case args of
    [inputFile,outputQual] ->
              bracket (openFile inputFile ReadMode) hClose $ \input -> 
                  bracket (openFile outputQual WriteMode) hClose $ \output ->
                      conversor input output
    [inputFile] -> bracket (openFile inputFile ReadMode) hClose $ \input -> conversor input stdout
    otherwise -> conversor stdin stdout

--- Apply f x y to snd of x and y
applySnd2 :: (b -> b -> c) -> (a,b) -> (a,b) -> c
applySnd2 f x y = f (snd x) (snd y)

