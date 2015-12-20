module Image where

import Data.Complex
import Graphics.GD
import Mandelbrot

mandelImg :: Double -> IO Image
mandelImg resolution =
    newImage (truncate resolution * 3 + 1, truncate resolution * 2 + 1)

mandelPoints :: Double -> [(Complex Double, Point)]
mandelPoints resolution =
    zip (mandelDomain resolution) (mandelRange $ truncate resolution)

mandelDomain :: Double -> [Complex Double]
mandelDomain resolution =
    [ (x / resolution) :+ (y / resolution)
    | y <- [ (-resolution) .. resolution ]
    , x <- [ (-2 * resolution) .. resolution ]
    ]

mandelRange :: Int -> [Point]
mandelRange resolution =
    [ (x, y)
    | y <- reverse [ 0 .. (2 * resolution) ]
    , x <- [ 0 .. (3 * resolution) ]
    ]

-- Run this by doing `echo <num> | runghc Image.hs`
main :: IO ()
main =
    let
        colorMandelPoint :: Image -> (Complex Double, Point) -> IO ()
        colorMandelPoint buffer (c, xy) =
            if mandelbrotTest c then
                setPixel xy (rgb 255 255 255) buffer
            else
                setPixel xy (rgb 0 0 0) buffer
    in do
        resolution <- readLn :: IO Double
        buffer <- mandelImg resolution
        mapM_ (colorMandelPoint buffer) $ mandelPoints resolution
        savePngFile "./mandelbrot.png" buffer