module Lib where

import Util

import Data
import Graphics
import Data.List (zip4, transpose)


-- Animation library (ffmpeg bindings)
import Codec.FFmpeg
import Codec.FFmpeg.Juicy (imageWriter)



__test_params = EncodingParams
                        { epWidth = 1280
                        , epHeight = 720
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
    bar_data <- return (reverse (bar_compute 48 soundData))
    max_data <- return (reverse $ (profile_compute 48 soundData))
    mapM_ writer [(Just . bar_plot_480)  bd | bd <- zip4 bar_data [1..] (repeat (length bar_data)) max_data]
    writer Nothing
    return ()
    where 
        sps = epFps __test_params
        setup_writer = default_writer outfile

