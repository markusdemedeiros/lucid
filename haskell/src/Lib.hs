module Lib
  ( runAudioPipelineTest,
  )
where

import Conduit (mapMC, mapM_C, runConduit, runConduitRes, takeC, toConsumer, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import Dataflow
import Processing (exampleFilter)
import Reanimate (mkCircle, raster, reanimate)
import Reanimate.Raster (mkImage)
import Reanimate.Render (Format (RenderMp4), Raster (RasterAuto), render)
import Sound.File.Sndfile (Sample)
import Sources

{-
 - Main interface for complete pipelins of actions
 -
 -}

runAudioPipelineTest :: IO ()
runAudioPipelineTest =
  do
    fileconduit <- startFileStream "./data/test_data.flac" :: IO (SoundStream Double)
    runConduitRes (exampleFilter fileconduit .| renderingConduit 0.2 (\s -> mkImage 200 200 "./data/Haskell-Logo.svg")) >>= renderanim
  where
    renderanim animation = render animation "./data/output.mp4" RasterAuto RenderMp4 400 300 60 False
