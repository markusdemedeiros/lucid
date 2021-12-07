{-# LANGUAGE ScopedTypeVariables #-}

module Data (read_sound
            , SlicedSound
            , seperate_freqs
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


-- | split a frequency into n even ranges, discarding from the top, and take the norm of each
seperate_freqs :: Int -> SampleFrequency -> [SampleFrequency]
seperate_freqs buckets v = SV.sliceVertical chunksize v
    where chunksize = (SV.length v) `div` buckets

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
bar_compute num_bars =  map (take num_bars . (map sup_norm) . seperate_freqs (num_bars+20)) . 
                            freqs_rescale . frequency


-- | Compute profile (long time average) data for the n-bar visualization: output
-- | is the average over 8 timeslices of the bar_compute data. Seperate function
-- | it needs different preprocessing to look good. 
profile_compute :: Int -> SlicedSound -> [[Double]]
profile_compute num_bars =  transpose . map (rolling_max 8) . transpose . map (take num_bars . (map sup_norm) . seperate_freqs (num_bars+20)) . freqs_rescale . frequency


-- | Normalization of volume within a single frame. Each value in the output list is
-- | the percentage value of the list's sum that position of the input list
in_frame_normalize :: [Double] -> [Double]
in_frame_normalize fs = map (/ total) fs 
    where total = foldr1 (+) fs
