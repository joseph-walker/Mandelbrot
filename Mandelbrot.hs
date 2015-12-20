module Mandelbrot where

import Data.Complex

mandelbrot :: Complex Double -> Complex Double -> Complex Double
mandelbrot c z =
    z ^^ (2 :: Int) + c

mandelbrotSequence :: Complex Double -> [Complex Double]
mandelbrotSequence c =
    iterate (mandelbrot c) (0 :+ 0)

mandelbrotTest :: Complex Double -> Bool
mandelbrotTest c
    | length testSequence == maximumIterations =
        True
    | otherwise =
        False
        where
            testSequence =
                take maximumIterations $ takeWhile (<= upperBound) sequenceMagnitude
            sequenceMagnitude =
                map magnitude $ mandelbrotSequence c
            maximumIterations =
                100
            upperBound =
                1e6