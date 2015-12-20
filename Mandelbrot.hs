module Mandelbrot where

import Data.Complex

mandelbrot :: Complex Double -> Complex Double -> Complex Double
mandelbrot c z =
    z ^^ (2 :: Int) + c

mandelbrotSequence :: Complex Double -> [Complex Double]
mandelbrotSequence c =
    iterate (mandelbrot c) (0 :+ 0)

mandelbrotVelocity :: Complex Double -> Int
mandelbrotVelocity c =
    length testSequence
        where
            testSequence =
                take maximumIterations $ takeWhile (<= upperBound) sequenceMagnitude
            sequenceMagnitude =
                map magnitude $ mandelbrotSequence c
            maximumIterations =
                100
            upperBound =
                1e6

mandelbrotTest :: Complex Double -> Bool
mandelbrotTest c
    | mandelbrotVelocity c == maximumIterations =
        True
    | otherwise =
        False
        where
            maximumIterations =
                100

zoomWindow :: (Double, Double) -> Int -> Int -> Double -> [(Complex Double, (Int, Int))]
zoomWindow topLeft width height zoomFactor =
    [ (calcPoint (fst topLeft) x :+ calcPoint (snd topLeft) y, (x, y))
    | y <- [0 .. height]
    , x <- [0 .. width]
    ]
    where
        calcPoint offset x =
            offset + (fromIntegral x * zoomFactor)