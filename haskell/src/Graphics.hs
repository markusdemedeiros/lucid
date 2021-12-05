module Graphics where


-- Static graphics library (JuicyPixels) for individual frame drawing
import Codec.Picture

-- Animation library (ffmpeg bindings)
import Codec.FFmpeg
import Codec.FFmpeg.Juicy (imageWriter)




{- 
 - Graphics for the bar chart
 -}

draw_bar_frame :: [Double] -> Image Pixel8
draw_bar_frame = todo







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
