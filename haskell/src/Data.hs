{-# LANGUAGE ScopedTypeVariables #-}

module Data (read_sound, SlicedSound, seperate_freqs, freqs_rescale, one_norm, bar_compute) where

import Util

-- Math Libraries
import Data.Complex 
import Math.FFT

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


type SoundData = BV.Buffer Double
-- interp: A blob (buffer) of raw sound data. Shouldn't leave this module- only
-- vectors leave this module- but we need a buffer to take Fourier transforms.


-- |'read_wav' (internal) reads raw file IO data 
-- | signals an error if the file cannot be read, or is in an incorrect format
read_wav :: FilePath -> IO ((SF.Info, SoundData))
read_wav file = do
    (info, mb_data) <- SF.readFile file
    case mb_data of 
        Just (dat :: SoundData) -> return $ (info, dat)
        _ -> error "error: file read error"
    


-- Synonyms for 
type SampleFrequency = SV.Vector (Complex Double)
type SampleAmplitude = SV.Vector Double

data SlicedSound = SlicedSound 
    { frequency :: [SampleFrequency] 
    , amplitude :: [SampleAmplitude] 
    , framerate :: Int
    }

-- interp. a file sliced into timeslices 
-- frequency
-- amplitude
-- framerate


-- Slice vector into list of vectors, coercing input sample rate to output sample rate
slice_vector :: (FS.Storable a) => Int -> Int -> SV.Vector a -> [SV.Vector a]
slice_vector in_sps ot_sps vec = SV.sliceVertical chunksize vec
    where chunksize = in_sps `div` ot_sps



read_sound :: FilePath -> Int -> IO (SlicedSound)
read_sound file out_sps = do
    (info, dat) <- read_wav file
    let slice = slice_vector (samplerate info) out_sps in
        return $ SlicedSound { amplitude = slice . BV.fromBuffer $ dat
                             , frequency = fmap (TCA.from .  dftRC . TCA.to) . slice . BV.fromBuffer $ dat
                             , framerate = out_sps }
    



{-
 - File Processing
 -
 - Helper functions to get different views of SlicedSound data, to be used directly by the graphics functions
 -}


-- |split a frequency into n even ranges, discarding from the top, and take the norm of each
seperate_freqs :: Int -> SampleFrequency -> [SampleFrequency]
seperate_freqs buckets v = SV.sliceVertical chunksize v
    where chunksize = (SV.length v) `div` buckets

-- |compute the 1-norm of a range of frequencies (average)
one_norm :: SampleFrequency -> Double
one_norm samples = (/(fromIntegral(SV.length samples))) . SV.foldl1' (+) . SV.map magnitude $ samples

-- |renormalize an entire set of frequency data so it's maximum value has magnitude 1
freqs_rescale :: [SampleFrequency] -> [SampleFrequency]
freqs_rescale samples = map (SV.map (/(max_magnitude :+ 0))) $ samples
    where max_magnitude = maximum . map (SV.maximum . SV.map magnitude) $ samples






bar_compute :: Int -> SlicedSound -> [[Double]]
bar_compute num_bars = map ((map one_norm) . seperate_freqs num_bars) . freqs_rescale . frequency
