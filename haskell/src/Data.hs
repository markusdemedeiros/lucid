{-# LANGUAGE ScopedTypeVariables #-}

module Data (read_sound, SlicedSound, seperate_freqs, freqs_rescale, sup_norm, bar_compute) where

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


__t :: IO()
__t = do
    (info1, Just (d1 :: SoundData)) <- SF.readFile "./data/not-a-rickroll.wav"
    print info1
    (info, Just (d :: SoundData)) <- SF.readFile "./data/not-a-rickroll_mono.wav"
    print info


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
    

-- | Average channels together in multi-channel tracks
-- (BROKEN- WAV FORMAT IS MORE COMPLEX THEN I THOUGHT. USE FFMPEG ON THE FRONT LAYER INSTEAD)
unchannel :: (SF.Info, SoundData) -> (SF.Info, SV.Vector Double)
unchannel (info, dat) = (new_info, new_data)
    where new_frames = frames info `div` channels info 

          dat_unpacked :: SV.Vector Double
          dat_unpacked = BV.fromBuffer dat
          
          -- gets the n'th (averaged) frame from dat_unpacked
          -- starts at 0
          get_frame :: Int -> Double
          get_frame n = (/ fromIntegral (channels info)) . 
                        foldr (+) 0 . 
                        map (SV.index dat_unpacked) $ [(channels info)*n..(channels info)*(n+1)-1]
          
          new_data :: SV.Vector Double
          new_data = SV.pack . map get_frame $ [0 .. new_frames - 1]

          -- roses are red
          -- LED's are brightening
          -- I wish I understood lenses
          -- but (<<%@=) is f****** frightening
          new_info :: SF.Info
          new_info = Info { frames = new_frames
                          , samplerate = samplerate info `div` channels info
                          , channels = 1
                          , format = format info 
                          , sections = sections info
                          , seekable = seekable info }


{-
 - File Processing
 -
 - Helper functions to get different views of SlicedSound data, to be used directly by the graphics functions
 -}


-- |split a frequency into n even ranges, discarding from the top, and take the norm of each
seperate_freqs :: Int -> SampleFrequency -> [SampleFrequency]
seperate_freqs buckets v = SV.sliceVertical chunksize v
    where chunksize = (SV.length v) `div` buckets

-- |compute the infinity norm of a range of frequencies (average)
sup_norm :: SampleFrequency -> Double
sup_norm samples = SV.maximum . SV.map magnitude $ samples

-- |renormalize an entire set of frequency data so it's maximum value has magnitude 1
freqs_rescale :: [SampleFrequency] -> [SampleFrequency]
freqs_rescale samples = map (SV.map (/(max_magnitude :+ 0))) $ samples
    where max_magnitude = maximum . map (SV.maximum . SV.map magnitude) $ samples


rolling_average :: Int -> [Double] -> [Double]
rolling_average n ls = ls''''
    where ls' = take (n-1) (repeat 0.0) ++ ls 
          ls'' = map (\n' -> drop n' ls') [0..n-1]
          ls''' = take (length ls) $ transpose ls''
          ls'''' = map ((/ fromIntegral n) . foldr1 (+)) ls'''



bar_compute :: Int -> SlicedSound -> [[Double]]
bar_compute num_bars = transpose . map (rolling_average 2) .  transpose . map ((map sup_norm) . tail . seperate_freqs (num_bars+1)) . freqs_rescale . frequency



