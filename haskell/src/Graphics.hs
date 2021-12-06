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

defaultImageMetadata = ImageMetadata 400 300



-- We only deal with one (fixed) pixel type
type Pix = Pixel8
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


bar_plot :: ImageMetadata -> [Double] -> Frame
bar_plot md ds = generateImage f (width md) (height md)
    where f x y 
            | (ds!!nbar) > 1 = error "shit"
            | (height md - y - 50) <= hbar = if (nbar `mod` 2) == 0 then 200 else 255
            | otherwise = 0
            where nbar = ((length ds) * x) `div` (width md)
                  hbar = round $ 1.25**(fromIntegral nbar) *(ds!!nbar)*(fromIntegral(height md))

__test_bp :: [Double] -> IO()
__test_bp d = writePng "./test_png_out.png" (bar_plot defaultImageMetadata d)

