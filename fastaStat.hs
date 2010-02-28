module Main
where

import Bio.Sequence
import System
import Control.Monad (forM,liftM)
import Data.Array.Vector
import Statistics.Quantile
import Statistics.Function

-- seq2len :: Sequence a -> IO Double

seq2DoubleLen :: Sequence Unknown -> Double
seq2DoubleLen x = (fromIntegral (seqlength x)) :: Double

quartile n x = (continuousBy s n 4 x)

stat :: UArr Double -> (Double,Double,Double,Double,Double)
stat x = let mm = minMax x
         in ((fstS mm),(quartile 1 x),(quartile 2 x),(quartile 3 x),(sndS mm))

main = do 
  [inputFile] <- getArgs
  seqs <- (readFasta inputFile)
  let lens = map seq2DoubleLen seqs
  let (min,n25,n50,n75,max) = stat (toU lens)
  putStrLn $   "min = " ++ (show min) ++
             "\tN25 = " ++ (show n25) ++ 
             "\tN50 = " ++ (show n50) ++
             "\tN75 = " ++ (show n75) ++
             "\tmax = " ++ (show max)
