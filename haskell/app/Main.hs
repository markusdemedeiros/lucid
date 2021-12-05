module Main where

import Lib
import System.Environment
import System.Exit

-- ffmpeg command: ffmpeg -i video.mp4 -i audio.wav -c:v copy -c:a aac output.mp4

main :: IO ()
main = main_cli


-- Based on the Argument Handling tutorial on the Haskell Wiki
-- https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling
main_cli :: IO()
main_cli = getArgs >>= parse >>= putStr

parse :: [String] -> IO String
parse ["-h"] = usage   >> exit_good
parse ["-v"] = version >> exit_good
parse []     = usage   >> exit_die
parse fs     = concat `fmap` mapM readFile fs

usage           = putStrLn "Usage: lucid [-vh] [file ..]"
version         = putStrLn "LucidVisualization version 0.1"
exit_good       = exitWith ExitSuccess
exit_die        = exitWith (ExitFailure 1)
