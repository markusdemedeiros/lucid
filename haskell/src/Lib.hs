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


-- Helper: Paritions a list into N segments of approximately equal length
linspace :: Int -> [a] -> [[a]]
linspace i l = linspace' (lindex i (length l)) l
    where linspace' :: [Int] -> [a] -> [[a]]
          linspace' [] x            = []
          linspace' (i:is) x     
                | (length x == 0)   = [x]
                | otherwise         = (take i x):(linspace' (map (\x -> x-i) is) (drop i x))
        
-- Helper: Creates a list of approximately equally spaced integers from 0 to N-1, 
-- excluding 0
lindex :: Int -> Int -> [Int]
lindex segments len = [round (i*sp') | i <- (map fromIntegral [1 .. segments])]
    where sp' = (fromIntegral len)/(fromIntegral segments)


linslice_with :: (FS.Storable a, FS.Storable b) => Int -> (a->b) -> CArray Int a -> [CArray Int b]
linslice_with  n f car = map mkslice $ zip (0:is) is
    where is = lindex n (size car)
          mkslice (l,h) = CArr.sliceWithP (l, h-1) (0,0) f car


linslice :: (FS.Storable a) => Int -> CArray Int a -> [CArray Int a]
linslice i c = linslice_with i id c 



test_grid :: Image Pixel8
test_grid = generateImage f test_w test_h 
    where f x y = fromIntegral $ max (8*x `mod` 255) (8*y `mod` 255)
          test_w = 800
          test_h = 600


writeTest :: Image Pixel8 -> IO () 
writeTest = writePng "output.png" 




__file_path :: FilePath
__file_path = "./data/sine.wav"

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
fft :: CArray Int Double -> CArray Int (Complex Double)
fft = dftRC

spectra_sum :: CArray Int (Complex Double) -> Double
spectra_sum = norm2

--             Sample rate of source
--                    Samples per second
make_slices :: Int -> Int -> CArray Int Double -> [CArray Int Double]
make_slices srsrc srout k = linslice approx_num_frames k
    where approx_num_frames = round ((fromIntegral(size k)) * fromIntegral(srout)/fromIntegral(srsrc))

__spectro :: IO ()
__spectro = do
    (info, Just (x :: BV.Buffer Double)) <- SF.readFile __file_path
    r <- return $ TCA.to $ BV.fromBuffer x
    t <- return $ make_slices (samplerate info) 100 r
    putStrLn . show $ t
    -- putStrLn . show $ generate_slices (ReadParamaters 1) info
    -- t <- return $  map ((flip subslice) r) $ generate_slices (ReadParamaters 1) info
    -- putStrLn . show $ t
    -- buckets <- return $ map (subslice_frac 1) t
    -- mapM (putStrLn . show) buckets
    return ()
   




