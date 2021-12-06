module Lib where

import Util

import Data
import Graphics

-- Animation library (ffmpeg bindings)
import Codec.FFmpeg
import Codec.FFmpeg.Juicy (imageWriter)


__file_path :: FilePath
__file_path = "./data/africa-toto.wav"

af_render :: IO()
af_render = render_bar_plot __file_path "big-africa-render.mp4"


__test_params = EncodingParams
                        { epWidth = 400
                        , epHeight = 300
                        , epFps = 20
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
    mapM_ writer [Just (plotter bd) | bd <- bar_compute num_bars soundData]
    writer Nothing
    return ()
    where 
        sps = epFps __test_params
        setup_writer = default_writer outfile
        num_bars = 12
        plotter = bar_plot defaultImageMetadata


-- __spectro_test :: IO()
-- __spectro_test = do
--     spectrum_data <- __spectro
--     writeTest $ draw_freq $ normalize_fs $ spectrum_data

-- __encoding_params = EncodingParams
--                         { epWidth = 400
--                         , epHeight = 300
--                         , epFps = 30
--                         , epCodec = Nothing
--                         , epPixelFormat = Nothing
--                         , epPreset = "medium"
--                         , epFormatName = Nothing }
