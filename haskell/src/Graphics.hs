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
            | y <= (height md - hbar) = 255
            | otherwise = 0
            where nbar = ((length ds) * x) `div` (width md) -- Width will span entire frame
                  hbar = round $ 5 * (ds!!nbar)*(fromIntegral(height md))

__test_bp :: [Double] -> IO()
__test_bp d = writePng "./test_png_out.png" (bar_plot defaultImageMetadata d)


-- render_MImage :: MImage -> FImage -> FImage
-- render_MImage bkg = (\x y -> mask_f (f x y))
--     where 
--           mask_f Nothing    = bkg x y
--           mask_f (Just x)   = const x



-- 
-- __writer :: IO (Maybe (Image Pixel8) -> IO ())
-- __writer = imageWriter __encoding_params __encoding_out
-- 
-- __frames_video :: IO()
-- __frames_video = do
--     spectrum_data <- __spectro
--     wr <- __writer
--     mapM_ wr $ map (Just . __draw_frame) $ normalize_fs $ spectrum_data
--     wr Nothing
-- 
-- 
-- __draw_frame :: [Pixel8] -> Image Pixel8
-- __draw_frame df = generateImage f test_w test_h
--     where test_w = 400
--           test_h = 300
--           f x y = df!!((length df) * x `div` 400)
-- 
-- 
-- __encoding_params :: EncodingParams
-- __encoding_params = defaultParams 400 300
-- 
-- -- __encoding_params = EncodingParams
-- --                         { epWidth = 400
-- --                         , epHeight = 300
-- --                         , epFps = 30
-- --                         , epCodec = Nothing
-- --                         , epPixelFormat = Nothing
-- --                         , epPreset = "medium"
-- --                         , epFormatName = Nothing }
-- 
-- test_grid :: Image Pixel8
-- test_grid = generateImage f test_w test_h 
--     where f x y = fromIntegral $ max (8*x `mod` 255) (8*y `mod` 255)
--           test_w = 800
--           test_h = 600
-- 
-- writeTest :: Image Pixel8 -> IO () 
-- writeTest = writePng "output.png" 
-- 
-- draw_freq :: [[Pixel8]] -> Image Pixel8
-- draw_freq df = generateImage f (scale*test_w) (scale*test_h)
--     where test_w = length df
--           test_h = length (df!!0)
--           f x y = (df!!(x `div` scale))!!(y`div`scale)
--           scale=5
-- 
--
--
--
-- normalize_fs :: [[Double]] -> [[Pixel8]] 
-- normalize_fs fs = map (map norze) $ fs
--     where -- fsl     = map (map log) $ fs
--           abs_max = maximum . map maximum $ fs
--           abs_min = minimum . map minimum $ fs
--           norze d = fromIntegral (round((d+abs_min)*255/abs_max)) :: Pixel8
-- 
