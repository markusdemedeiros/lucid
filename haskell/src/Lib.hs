module Lib where

import Util
import Data
import Graphics

-- List libraries
import Data.List (zip4, transpose)

-- Ffmpeg bindings
import Codec.FFmpeg
import Codec.FFmpeg.Juicy (imageWriter)


{-
 - Main rendering code and FFMPEG bindings
 -}

-- | Default FFMPEG settings for output video
encoding_defaults = EncodingParams
                        { epWidth = 640
                        , epHeight = 480
                        , epFps = 20
                        , epCodec = Nothing
                        , epPixelFormat = Nothing
                        , epPreset = "medium"
                        , epFormatName = Nothing }


-- | Read a .wav file infile, and produce a .mp4 file containing the animation outfile
render_bar_plot  :: FilePath -> FilePath -> IO()
render_bar_plot infile outfile = do
    soundData <- read_sound infile $ epFps encoding_defaults 
    writer <- imageWriter encoding_defaults outfile
    let bar_data = reverse $ bar_compute 48 soundData
        max_data = reverse $ profile_compute 48 soundData
        in do mapM_ writer [(Just . bar_plot_480_48)  bd | bd <- zip4 bar_data [1..] (repeat (length bar_data)) max_data]
              writer Nothing
              return ()

