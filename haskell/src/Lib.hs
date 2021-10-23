module Lib
  ( runAudioPipelineTest,
  )
where

import Conduit (mapMC, mapM_C, runConduit, runConduitRes, takeC, toConsumer, (.|))
import Control.Monad.Trans.Resource (runResourceT)
import Dataflow
import Processing (exampleFilter)
import Reanimate (mkCircle, raster, reanimate)
import Reanimate.Animation (Animation, SVG)
import Reanimate.Raster (mkImage)
import Reanimate.Render (Format (RenderMp4), Raster (RasterAuto), render)
import Sound.File.Sndfile (Sample)
import Sources

{-
 - Main interface for complete pipelins of actions
 -
 -}

basicAudioPipeline :: FilePath -> (SoundStream a -> DataStream Double) -> (Double -> SVG) -> IO ()
basicAudioPipeline path filter frameanimation = do
  fileconduit <- startFileStream path :: IO (SoundStream Double)
  runConduitRes (exampleFilter fileconduit .| renderingConduit 0.2 frameanimation) >>= renderanim
  where
    renderanim animation = render animation "./data/output.mp4" RasterAuto RenderMp4 400 300 60 False

runAudioPipelineTest :: IO ()
runAudioPipelineTest =
  basicAudioPipeline
    "./data/test_data.flac"
    exampleFilter
    (\s -> mkImage (50 + 2 * s) (50 + 2 * s) "./data/Haskell-Logo.svg")