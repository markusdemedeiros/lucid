{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Data.Complex 

import qualified Data.StorableVector as SV
import Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector as BV
import qualified Data.StorableVector.CArray as TCA

import Math.FFT

import Data.Array.CArray
import qualified Data.Array.IArray as Arr

-- For testing only
import Graphics.EasyPlot

__file_path :: FilePath
__file_path = "./data/africa-toto.wav"

__test :: IO () -- (CArray Int Double)
__test = do
    (info, Just (x :: BV.Buffer Double)) <- SF.readFile __file_path
    putStrLn $ show info
    -- return $ TCA.to $ BV.fromBuffer x
    return () 



-- __fft_test  :: IO (CArray Int (Complex Double))
-- __fft_test = do
--     __data <- __test
--     return $ dftRC __data
-- 
-- 
-- __graph_ft :: IO()
-- __graph_ft = do
--     fft_data <- __fft_test
--     plot X11 $ Data2D [Title "FFT of a sin wave"] [] (zip [1..] $ SV.unpack $ TCA.from fft_data)
--     return ()








{- FRAGMENTS
 -
 - How to open a file
 -
 -  SF.openFile __file_path SF.ReadMode SF.defaultInfo  :: IO Handle
 -
 -
 -
 -  Get file info (frames, sampleRate, channels, etc).
 -
 -      SF.hInfo 
 -
 -
 -
 -
 -
 -
 -
 -}

