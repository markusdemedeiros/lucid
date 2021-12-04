{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Util
import Control.Monad (mapM_)

-- Math Libraries
import Data.Complex 
import Math.FFT
import qualified Foreign.Storable                           as FS

-- Wrapper library for audio files
import Sound.File.Sndfile                                   as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector   as BV

-- Vector processing libraries
import qualified Data.StorableVector                        as SV
import qualified Data.StorableVector.CArray                 as TCA
import qualified Data.Array.CArray                          as CA


-- Static graphics library
import Codec.Picture

-- Animation library (ffmpeg bindings)
import Codec.FFmpeg
import Codec.FFmpeg.Juicy (imageWriter)

__encoding_params :: EncodingParams
__encoding_params = defaultParams 400 300

__encoding_out :: FilePath
__encoding_out = "vid-output.mp4"

__writer :: IO (Maybe (Image Pixel8) -> IO ())
__writer = imageWriter __encoding_params __encoding_out

__file_path :: FilePath
__file_path = "./data/africa-toto.wav"

__spectro_test :: IO()
__spectro_test = do
    spectrum_data <- __spectro
    writeTest $ draw_freq $ normalize_fs $ spectrum_data

__frames_video :: IO()
__frames_video = do
    spectrum_data <- __spectro
    wr <- __writer
    mapM_ wr $ map (Just . __draw_frame) $normalize_fs $ spectrum_data
    wr Nothing



__draw_frame :: [Pixel8] -> Image Pixel8
__draw_frame df = generateImage f test_w test_h
    where test_w = 400
          test_h = 300
          f x y = df!!((length df) * x `div` 400)



-- __encoding_params = EncodingParams
--                         { epWidth = 400
--                         , epHeight = 300
--                         , epFps = 30
--                         , epCodec = Nothing
--                         , epPixelFormat = Nothing
--                         , epPreset = "medium"
--                         , epFormatName = Nothing }


-- Computes the DFT of a TimeSlice
fft :: SV.Vector Double -> SV.Vector (Complex Double)
fft = TCA.from . dftRC . TCA.to

spectra_sum :: CA.CArray Int (Complex Double) -> Double
spectra_sum = CA.norm2

-- Split frequency into n even ranges, discarding from the top, and take the norm of each
bucket_freqs :: Int -> SV.Vector (Complex Double) -> [Double]
bucket_freqs buckets v = map (CA.norm2 . TCA.to) $ SV.sliceVertical chunksize v
    where chunksize = (SV.length v) `div` buckets


-- Helper: Takes input samples per second, output samples per second, and chunks a
--  vector into evenly spaced chunks for the output rate
--
--  The last frame may be smalller. 
timeslice_vector :: (FS.Storable a) => Int -> Int -> SV.Vector a -> [SV.Vector a]
timeslice_vector in_sps ot_sps vec = SV.sliceVertical chunksize vec
    where chunksize = in_sps `div` ot_sps







__spectro :: IO ([[Double]])
__spectro = do
    (info, Just (x :: BV.Buffer Double)) <- SF.readFile __file_path
    -- putStrLn $ show $ SV.length $ BV.fromBuffer x
    tsv <- return $ timeslice_vector (samplerate info) 2 $ BV.fromBuffer x
    fbs <- return $ fmap (bucket_freqs 100 . fft) tsv
    -- mapM (putStrLn . show . map (round . log)) fbs
    return fbs

test_grid :: Image Pixel8
test_grid = generateImage f test_w test_h 
    where f x y = fromIntegral $ max (8*x `mod` 255) (8*y `mod` 255)
          test_w = 800
          test_h = 600

writeTest :: Image Pixel8 -> IO () 
writeTest = writePng "output.png" 

normalize_fs :: [[Double]] -> [[Pixel8]] 
normalize_fs fs = map (map norze) $ fs
    where -- fsl     = map (map log) $ fs
          abs_max = maximum . map maximum $ fs
          abs_min = minimum . map minimum $ fs
          norze d = fromIntegral (round((d+abs_min)*255/abs_max)) :: Pixel8

draw_freq :: [[Pixel8]] -> Image Pixel8
draw_freq df = generateImage f (scale*test_w) (scale*test_h)
    where test_w = length df
          test_h = length (df!!0)
          f x y = (df!!(x `div` scale))!!(y`div`scale)
          scale=5


take_every_nth :: Int -> [a] -> [a]
take_every_nth _ [] = []
take_every_nth n l  = (head l):(take_every_nth n (drop n l))
