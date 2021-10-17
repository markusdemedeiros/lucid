module Processing where

import Conduit (mapC, (.|))
import Data.Conduit.Audio
import qualified Data.Vector.Storable as V
import Dataflow
import Sound.File.Sndfile (Sample)

{-
 - Functions for stream processing
 -
 - The functions here should take (Monad m) => AudioSource m a
 - and target
 -
 -}

-- Creates a data stream of the HEAD SAMPLE in each sound slice
exampleFilter :: (Sample a) => SoundStream a -> DataStream a
exampleFilter = applyVectorFilter V.head

-- Creates a data stream of the LENGTHS of sample vectors
exampleFilter2 :: (Sample a) => SoundStream a -> DataStream Int
exampleFilter2 = applyVectorFilter V.length

-- Functions from Data.Conduit.Audio can be directly applied to SoundStream data