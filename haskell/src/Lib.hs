module Lib where

import Util

__file_path :: FilePath
__file_path = "./data/africa-toto.wav"

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
-
