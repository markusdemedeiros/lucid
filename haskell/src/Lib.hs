module Lib
  ( runAudioPipelineTest,
  )
where

import Conduit (mapMC, mapM_C, runConduit, runConduitRes, takeC, toConsumer, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import Dataflow
import Processing (exampleFilter)
import Reanimate (mkCircle, raster, reanimate)
import Reanimate.Render (Format (RenderMp4), Raster (RasterAuto), render)
import Sound.File.Sndfile (Sample)
import Sources

{-
 - Main interface for animation actions
 -
 -
 -}

runAudioPipelineTest :: IO ()
runAudioPipelineTest =
  do
    fileconduit <- startFileStream "./data/test_data.flac" :: IO (SoundStream Double)
    runConduitRes (exampleFilter fileconduit .| renderingConduit 0.2 (\s -> mkCircle 1.0)) >>= renderanim
  where
    renderanim animation = render animation "./data/output.mp4" RasterAuto RenderMp4 400 300 60 False
