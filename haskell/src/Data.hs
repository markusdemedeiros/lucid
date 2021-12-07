{-# LANGUAGE ScopedTypeVariables #-}

module Data (read_sound
            , SlicedSound
            , slice_vector
            , freqs_rescale
            , sup_norm
            , bar_compute
            , rolling_max
            , profile_compute
            , rolling_average
            , in_frame_normalize
            ) where

import Util

-- Math Libraries
import Data.Complex 
import Math.FFT
import Data.List (transpose)

-- Additional vector processing libraries
import qualified Data.StorableVector.CArray                 as TCA
import qualified Data.Array.CArray                          as CA

-- Wrapper library for audio files
import Sound.File.Sndfile                                   as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector   as BV

-- Vector processing libraries 
import qualified Data.StorableVector                        as SV
import qualified Foreign.Storable                           as FS


{-
 - File IO
 -}


-- | A blob (buffer) of raw sound data. Shouldn't leave this module- only
-- | vectors leave this module- but we need a buffer to take Fourier transforms.
type SoundData = BV.Buffer Double


-- |'read_wav' (internal) reads raw file IO data 
-- | signals an error if the file cannot be read, or is in an incorrect format
read_wav :: FilePath -> IO ((SF.Info, SoundData))
read_wav file = do
    (info, mb_data) <- SF.readFile file
    case mb_data of 
        Just (dat :: SoundData) -> return $ (info, dat)
        _ -> error "error: file read error"


-- | Frequency representation of an audio file
type SampleFrequency = SV.Vector (Complex Double)


-- | Amplitude (sample) representation of an audio file
type SampleAmplitude = SV.Vector Double


-- | Sliced representation of an audio file, containing both the raw samples,
-- |  the data in the frequency domain, and the playback rate
data SlicedSound = SlicedSound 
    { frequency :: [SampleFrequency] 
    , amplitude :: [SampleAmplitude] 
    , framerate :: Int
    }


-- | Slice a vector into timeslices, given an input sample rate and a desired
-- | output rate
-- |    REQUIRES: in_sps <= ot_sps 
slice_vector :: (FS.Storable a) => Int -> Int -> SV.Vector a -> [SV.Vector a]
slice_vector in_sps ot_sps vec = SV.sliceVertical chunksize vec
    where chunksize = in_sps `div` ot_sps


-- | Read a .wav file from disk, produce timesliced data in the sample and
-- |    frequency domain with a specified output timeslice sample rate
read_sound :: FilePath -> Int -> IO (SlicedSound)
read_sound file out_sps = do
    (info, dat) <- read_wav file
    let slice = slice_vector (samplerate info) out_sps in
        return $ SlicedSound { amplitude = slice . BV.fromBuffer $ dat
                             , frequency = fmap (TCA.from .  dftRC . TCA.to) . slice . BV.fromBuffer $ dat
                             , framerate = out_sps }
    

{-
 - Data Processing
 -
 -}


-- | split a frequency into n even ranges, discarding from the top
seperate_freqs :: Int -> SampleFrequency -> [SampleFrequency]
seperate_freqs buckets v = SV.sliceVertical chunksize v
    where chunksize = (SV.length v) `div` buckets


-- | Seperates frequencies into n geometric ratio ranges, and takes their sup norm
seperate_freqs_nonlinear :: Int -> SampleFrequency -> [Double]
seperate_freqs_nonlinear n v = [ith_range i | i <- [0..(n-1)]]
    where ith_range :: Int -> Double
          ith_range i = sup_norm $ SV.take (ub-lb) $ SV.drop lb $ v
            where lb = f_idx i
                  ub = min (lb+10) (f_idx (i+1))  
        
          -- | Function from [0,1] to [0,1] which compensates for human hearing range
          nonlinear_f :: Double -> Double 
          nonlinear_f x = (x**3 + x) / 2
         
          -- | Rescales an index to be within (0,1), applies f, rescales to be within (0, length v - 1)
          f_idx :: Int -> Int
          f_idx i = round $ (* fromIntegral (SV.length v - 1)) . nonlinear_f $ (fromIntegral i) / (fromIntegral n)
          


-- | compute the infinity norm of a range of frequencies (average)
sup_norm :: SampleFrequency -> Double
sup_norm samples = SV.maximum . SV.map magnitude $ samples

-- | renormalize an entire set of frequency data so it's maximum value has magnitude 1
freqs_rescale :: [SampleFrequency] -> [SampleFrequency]
freqs_rescale samples = map (SV.map (/(max_magnitude :+ 0))) $ samples
    where max_magnitude = maximum . map (SV.maximum . SV.map magnitude) $ samples


-- | Produce a list where every element is the average of the last n elements (or
-- | fewer, near the head of the list
-- |
-- | TODO: check this function again, and refactor.
rolling_average :: Int -> [Double] -> [Double]
rolling_average n ls = ls''''
    where ls' = take (n-1) (repeat 0.0) ++ ls 
          ls'' = map (\n' -> drop n' ls') [0..n-1]
          ls''' = take (length ls) $ transpose ls''
          ls'''' = map ((/ fromIntegral n) . foldr1 (+)) ls'''


-- | Produce a list where every element is the maximum of the last n elements (or
-- | fewer, near the head of the list
-- |
-- | TODO: check this function again, and refactor.
rolling_max :: Int -> [Double] -> [Double]
rolling_max n ls = ls''''
    where ls' = take (n-1) (repeat 0.0) ++ ls 
          ls'' = map (\n' -> drop n' ls') [0..n-1]
          ls''' = take (length ls) $ transpose ls''
          ls'''' = map (foldr1 max) ls'''


-- | Compute bar data for a n-bar visualization: output is a list of length n 
-- | lists of bucketed and renormalized samples in the frequency domain,
-- | corresponding to each timeslice.
bar_compute :: Int -> SlicedSound -> [[Double]]
bar_compute num_bars =  map (seperate_freqs_nonlinear num_bars) . 
                            freqs_rescale . frequency


-- | Compute profile (long time average) data for the n-bar visualization: output
-- | is the average over 8 timeslices of the bar_compute data. Seperate function
-- | it needs different preprocessing to look good. 
profile_compute :: Int -> SlicedSound -> [[Double]]
profile_compute num_bars =  transpose . map (rolling_max 8) . transpose . 
                            map (seperate_freqs_nonlinear num_bars) . 
                            freqs_rescale . frequency


-- | Normalization of volume within a single frame. Each value in the output list is
-- | the percentage value of the list's sum that position of the input list
in_frame_normalize :: [Double] -> [Double]
in_frame_normalize fs = map (/ total) fs 
    where total = foldr1 (+) fs
