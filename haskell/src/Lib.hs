{-# LANGUAGE ScopedTypeVariables #-}
module Lib where


import qualified Data.Vector.Storable as SV
import Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.Vector as BV


__file_path :: FilePath
__file_path = "./data/africa-toto.wav"


__test :: IO (SV.Vector Double)
__test = do
    (info, Just (x :: BV.Buffer Double)) <- SF.readFile __file_path
    putStrLn $ show info
    return $ BV.fromBuffer x




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

