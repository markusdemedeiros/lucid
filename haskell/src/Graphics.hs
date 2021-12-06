module Graphics (bar_plot, ImageMetadata, defaultImageMetadata, Frame) where

import Util

-- Static graphics library (JuicyPixels) for individual frame drawing
import Codec.Picture


import Control.Monad (foldM, mplus)


{- 
 - Graphics for the bar chart
 -}


data ImageMetadata = ImageMetadata { width :: Int
                                   , height :: Int
                                   }

defaultImageMetadata = ImageMetadata 852 480


-- We only deal with one (fixed) pixel type
type Pix = PixelRGB8
type Frame = Image Pix

-- -- Functional image
-- type FImage = Int -> Int -> Pix
-- 
-- -- Masked functional image
-- type MImage = Int -> Int -> Maybe Pix
-- 
-- -- One advantage of this representation: masking layers is really easy!
-- 
-- -- | Overlays image1 on top of image2
-- mask_overlay :: MImage -> MImage -> MImage
-- mask_overlay image1 image2 x y = mplus (image1 x y) (image2 x y)


bar_plot :: ImageMetadata -> ([Double], Int) -> Frame
bar_plot md (ds, t) = generateImage f (width md) (height md)
    where f x y 
            | (ds!!nbar) > 1 = error "out of range value check"
            | (height md - y - 20) <= hbar = rainbow!!mag
            | otherwise = (PixelRGB8 (fromIntegral (x+t) `mod` 255) 
                                     (fromIntegral (y+3*round(100 * sin(fromIntegral t / 40))) `mod` 255) 
                                     (fromIntegral(x*y`div`30) `mod` 255))
            where nbar = ((length ds) * x) `div` (width md)
                  hbar = round $ 0.3* (fromIntegral nbar) * (ds!!nbar)*(fromIntegral(height md))
                  mag = (round (fromIntegral nbar + (fromIntegral t) / 30)) `mod` lr
                  rainbow = [ PixelRGB8 255 0 0
                            , PixelRGB8 255 127 0
                            , PixelRGB8 255 255 0
                            , PixelRGB8 127 255 0
                            , PixelRGB8 0 255 0
                            , PixelRGB8 0 255 127
                            , PixelRGB8 0 255 255
                            , PixelRGB8 0 127 255
                            , PixelRGB8 0 0 255
                            , PixelRGB8 120 0 255
                            , PixelRGB8 255 0 255
                            , PixelRGB8 255 0 127 ]
                  lr = length rainbow


-- if (nbar `mod` 2) == 0 then (PixelRGB8 200 0 0) else (PixelRGB8 255 0 0)



