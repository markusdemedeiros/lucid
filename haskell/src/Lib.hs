module Lib where

import Util

import Data
import Graphics

-- Animation library (ffmpeg bindings)
import Codec.FFmpeg
import Codec.FFmpeg.Juicy (imageWriter)


__file_path :: FilePath
__file_path = "./data/africa-toto.wav"



__test_params = EncodingParams
                        { epWidth = 400
                        , epHeight = 300
                        , epFps = 4
                        , epCodec = Nothing
                        , epPixelFormat = Nothing
                        , epPreset = "medium"
                        , epFormatName = Nothing }




default_writer :: FilePath -> IO (Maybe Frame -> IO())
default_writer = imageWriter __test_params



render_bar_plot  :: FilePath -> FilePath -> IO()
render_bar_plot infile outfile = do
    -- read sound data from file
    soundData <- read_sound infile sps
     
    -- setup writer, send frames to file, close
    writer <- setup_writer 
    
    mapM_ writer [Just (plotter bd) | bd <- take 16 (bar_compute num_bars soundData)]

    writer Nothing
    return ()
    where 
        sps = 4
        setup_writer = default_writer outfile
        num_bars = 8
        plotter = bar_plot defaultImageMetadata





-- __frames_video :: IO()
-- __frames_video = do
--     spectrum_data <- __spectro
--     wr <- setup_writer
--     mapM_ wr $ map (Just . __draw_frame) $ normalize_fs $ spectrum_data
--     wr Nothing
--     where setup_writer = imageWriter 






-- 
-- __spectro_test :: IO()
-- __spectro_test = do
--     spectrum_data <- __spectro
--     writeTest $ draw_freq $ normalize_fs $ spectrum_data
-- 
-- 
-- 
-- 
-- __spectro :: IO ([[Double]])
-- __spectro = do
--     (info, Just (x :: BV.Buffer Double)) <- SF.readFile __file_path
--     -- putStrLn $ show $ SV.length $ BV.fromBuffer x
--     tsv <- return $ timeslice_vector (samplerate info) 2 $ BV.fromBuffer x
--     fbs <- return $ fmap (bucket_freqs 8 . fft) tsv
--     -- mapM (putStrLn . show . map (round . log)) fbs
--     return fbs
-- 
-- 
-- 
-- 
-- 




-- 
-- __encoding_out :: FilePath
-- __encoding_out = "vid-output.mp4"


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
