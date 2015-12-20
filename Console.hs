module Console where

import Data.List
import Data.Complex
import Mandelbrot

points :: Double -> [[Complex Double]]
points resolution =
    [[a :+ b | a <- domain] | b <- range]
        where
            domain =
                map (/ resolution) [(- resolution * 2) .. resolution]
            range =
                map (/ resolution) [(- resolution) .. resolution]

mandelbrotSet :: [[Complex Double]] -> [[Bool]]
mandelbrotSet =
    map . map $ mandelbrotTest

-- Run this by doing `echo <num> | runghc Console.hs`
main :: IO ()
main = do
    resolution <- readLn :: IO Double
    mapM_ (print . intersperse ' ' . map mandelChar) (mandelbrotSet $ points resolution)
    where
        mandelChar :: Bool -> Char
        mandelChar True  = 'o'
        mandelChar False = '-'