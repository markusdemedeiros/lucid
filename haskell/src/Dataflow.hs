module Dataflow where

import Conduit (ConduitM, ResourceT, foldlC, mapC, takeC, (.|))
import Control.Monad
import Data.Bitraversable (Bitraversable)
import Data.Conduit as C (ConduitT)
import Data.Conduit.Audio (AudioSource (AudioSource))
import qualified Data.Vector.Storable as V
import Reanimate (Animation, SVG, seqA, staticFrame)
import Reanimate.Animation (pause)
import Sound.File.Sndfile.Buffer (Sample)

{-
 - Dataflow
 -
 - The purpose of this module is to abstract away all the
 -  conduit functions on the stage boundaries, so that each
 -  stage can write simple pure functions
 -
 - "ConduitT a b m r"
 -      - Satan, 2021
 -
 -}

-- Base Monad for audio processing (from Data.Conduit.Audio, transformer requires a ResourceT on a monadIO)
type AudioEffectM = (ResourceT IO)

-- A SoundStream of type a is data streamed from a sound file, with metadata, that conduits sound data
type SoundStream a = AudioSource AudioEffectM a

-- An AudioProducer is a function which can give us a sound stream, with IO effects.
type AudioProducer x a = x -> IO (SoundStream a)

-- A DataStream of type a is a conduit which contains vectors of a
type DataStream a = ConduitT () a AudioEffectM ()

-- Pulls a stream of vectors of samples from a SoundStream
getStreamData :: (Sample a) => SoundStream a -> DataStream (V.Vector a)
getStreamData (AudioSource source _ _ _) = source

renderingConduit :: Double -> (v -> SVG) -> ConduitT v a AudioEffectM Animation
renderingConduit frameLength renderer =
  mapC (staticFrame frameLength . renderer) .| foldlC seqA (pause frameLength)

applyVectorFilter :: (Sample a) => (V.Vector a -> b) -> SoundStream a -> DataStream b
applyVectorFilter f ss = getStreamData ss .| mapC f
