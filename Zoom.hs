module Zoom where

import Mandelbrot
import Graphics.GD
import Data.Complex

colorComp :: Double -> Int -> Int
colorComp lambda x =
    truncate . (255 - ) $ 255 * (2.7161259 :: Double) ** (- lambda * fromIntegral x)

colorScale :: Int -> Color
colorScale x =
    rgb (colorComp 0.25 x) (colorComp 0.05 x) (colorComp 0.1 x)

colorMandelPoint :: Image -> (Complex Double, Point) -> IO ()
colorMandelPoint buffer (c, xy) =
    setPixel xy (colorScale $ mandelbrotVelocity c) buffer

-- colorMandelPoint :: Image -> (Complex Double, Point) -> IO ()
-- colorMandelPoint buffer (c, xy) =
--     if mandelbrotTest c then
--         setPixel xy (rgb 255 255 255) buffer
--     else
--         setPixel xy (rgb 0 0 0) buffer

main :: IO ()
main =
    let dim = 800 in do
        buffer <- newImage (dim, dim)
        mapM_ (colorMandelPoint buffer) $ zoomWindow (-0.6, 0.4) dim dim 0.0005
        savePngFile "./zoom.png" buffer