{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Sources where

import qualified Data.Conduit.Audio.Sndfile as SF
import Dataflow (SoundStream)
import Sound.File.Sndfile.Buffer (Sample)

-- The conduit-audio-sndfile gives us a way to turn a filepath into a SoundSource
startFileStream :: (Sample a) => FilePath -> IO (SoundStream a)
startFileStream filepath = do
  putStrLn $ "starting an audio stream from file " ++ filepath
  SF.sourceSnd filepath