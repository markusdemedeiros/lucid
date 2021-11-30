{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Data.Complex 

import qualified Data.StorableVector as SV
import Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import qualified Data.StorableVector.CArray as TCA
import Math.FFT
import Data.Array.CArray as CArr
import qualified Data.Array.IArray as Arr
import qualified Foreign.Storable as FS

-- For testing only
import Codec.Picture

todo = undefined



__test :: IO (CArray Int Double)
__test = do
    (info, Just (x :: BV.Buffer Double)) <- SF.readFile __file_path
    putStrLn $ show info
    r <- return $ TCA.to $ BV.fromBuffer x
    putStrLn $ show (CArr.shape r)
    return r

__resize_test :: IO (CArray Int Double)
__resize_test = do
    x <- __test
    return $ CArr.sliceP (0, 10) (0, 0) x


data TimeSlice 
    = TimeSlice { sampleRate :: Int -- Want to keep values as integers if possible
                , lo :: Int
                , hi :: Int
                }
        deriving (Eq, Show)


data ReadParamaters
    = ReadParamaters { samplePerSecond :: Int   -- Number of samples to take per second 
                     }
        deriving (Eq, Show)


generate_slices :: ReadParamaters -> SF.Info -> [TimeSlice]
generate_slices rp si = [TimeSlice sr i (i+nf-1) | i <- [0, nf .. fr]]
    where nf = samplerate si `div` samplePerSecond rp 
          sr = samplerate si 
          fr = frames si


__test_generate_slices :: IO ()
__test_generate_slices = do 
    (info, Just (x :: BV.Buffer Double)) <- SF.readFile __file_path
    putStrLn . show $ info
    mapM (putStrLn . show) $ generate_slices (ReadParamaters 1) info
    return ()


-- Computes the amount of animation time a TimeSlice should occupy
sliceDuration :: TimeSlice -> Double
sliceDuration t = (hi' - lo') / sr'
    where hi' = fromIntegral $ hi t
          lo' = fromIntegral $ lo t
          sr' = fromIntegral $ sampleRate t

-- Extract a subslice of a CArray correponding to a TimeSlice
subslice :: TimeSlice -> CArray Int Double -> CArray Int Double
subslice ts = CArr.sliceP (lo ts, hi ts) (0,0)

-- -- Extract a list of subslices in k buckets, mapping to magnitude
-- subslice_frac :: Int -> CArray Int (Complex Double) -> [CArray Int Double]
-- subslice_frac k c = [(CArr.sliceWithP (rsz*n, rsz*(n+1)-1) (0,0) magnitude c) | n <- [0..k-1]] 
--     where rsz = (size c) `div` k
--           takeNorm :: CArray Int Double -> Double
--           takeNorm = normSup
    

-- Computes the DFT of a TimeSlice
fft :: SV.Vector Double -> SV.Vector (Complex Double) -- CArray Int Double -> CArray Int (Complex Double)
fft = TCA.from . dftRC . TCA.to

spectra_sum :: CArray Int (Complex Double) -> Double
spectra_sum = norm2

-- Split frequency into n even ranges, discarding from the top, and take the norm of each
bucket_freqs :: Int -> SV.Vector (Complex Double) -> [Double]
bucket_freqs buckets v = map (norm2 . TCA.to) $ SV.sliceVertical chunksize v
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

__file_path :: FilePath
__file_path = "./data/africa-toto.wav"

__spectro_test :: IO()
__spectro_test = do
    spectrum_data <- __spectro
    -- mapM (putStrLn . show . map round) $ normalize_fs spectrum_data
    writeTest $ draw_freq $ normalize_fs $ spectrum_data

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
