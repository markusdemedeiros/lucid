{-# LANGUAGE ScopedTypeVariables #-}

module Main where


import Lib
import Util
import Data.List (isSuffixOf)
import Data.Char (toUpper)

import System.Directory (doesFileExist, removeFile)
import System.Process
import System.Environment
import System.Exit
import System.IO

-- Used to check .wav file metadata, maybe refactor to data?
import qualified Sound.File.Sndfile as SF
import qualified Sound.File.Sndfile.Buffer.StorableVector   as BV


main :: IO ()
main = main_cli


-- Based on the Argument Handling tutorial on the Haskell Wiki
-- https://wiki.haskell.org/Tutorials/Programming_Haskell/Argument_handling
main_cli :: IO()
main_cli = getArgs >>= parse  >> exit_good
    where  parse :: [String] -> IO ()
           parse ["-h"]    = usage
           parse ["-v"]    = version
           parse ["-i", f] = visualize_file f
           parse _         = usage >> exit_die

usage           = putStrLn "Usage: lucid [-vhi] [file]"
version         = putStrLn "LucidVisualization version 0.1"
exit_good       = exitWith ExitSuccess
exit_die        = exitWith (ExitFailure 1)


visualize_file :: FilePath -> IO ()
visualize_file fp = do check_wav . check_exists . ask_overwrite $ return ()
                       temp_out <- gen_temp_file 
                       temp_pre <- gen_temp_audio
                       
                       -- fp is the input filepath: exists, is .wav file
                       -- temp_mono is the mono-audio filepath: does not exist
                       -- temp_out is the no-audio filepath: does no exist
                       -- out_file is the output file: does not exist
                       
                       -- Preprocess audio with FFMPEG call 
                       channels <- check_channels fp
                       er_ffmpeg <- rawSystem "ffmpeg" ["-i", fp, 
                                                        "-ac", "1",
                                                        temp_pre]
                       putStrLn $ ">> Preprocessed audio " ++ temp_pre ++ " written"
                       
                       -- Render the video frames
                       render_bar_plot temp_pre temp_out
                       putStrLn $ ">> Video " ++ temp_out ++ " written"
                       
                       -- Combine and cleanup
                       er_ffmpeg <- rawSystem "ffmpeg" ["-i", temp_out, 
                                                        "-i", temp_pre, 
                                                        "-c:v", "copy", 
                                                        "-c:a", "aac", 
                                                        out_file]
                       case er_ffmpeg of
                            ExitSuccess -> do
                                putStrLn (">> " ++ out_file  ++ " written") 
                                cleanup [temp_pre, temp_out]
                                exit_good
                            (ExitFailure n) -> do
                                putStrLn (">> FFMPEG error: " ++ show n) 
                                cleanup [temp_pre, temp_out]
                                exit_die



    where check_wav :: IO () -> IO () 
          check_wav next = if (isSuffixOf ".wav" fp) 
                            then next
                            else putStrLn ("error: file " ++ fp ++ " is not a .wav file") >> exit_die

          check_exists :: IO () -> IO ()
          check_exists next = do
                file_exists <- doesFileExist fp
                if file_exists 
                    then next
                    else putStrLn ("error: file " ++ fp ++ " does not exist") >> exit_die

          -- Strip ".wav" off of the end of the filepath, if it exists
          basefile :: FilePath
          basefile = reverse . drop 4 . reverse $ fp

          out_file :: FilePath
          out_file = basefile ++ ".mp4"

          ask_overwrite :: IO () -> IO ()
          ask_overwrite next = do
                file_exists <- doesFileExist out_file
                if file_exists 
                    then do putStrLn $ "output file " ++ out_file ++ " exists, overwrite?" 
                            ov <- get_yn
                            if ov 
                                then removeFile out_file >> next 
                                else exit_good
                    else next

          get_yn :: IO(Bool)
          get_yn = do
                putStr "[y/n]? "
                hFlush stdout
                c <- getLine
                case map toUpper c of
                    "Y" -> return True
                    "N" -> return False
                    otherwise -> get_yn
          
          -- temporary file to output video to
          gen_temp_file :: IO FilePath
          gen_temp_file = gen_temp_file' ".mp4" (basefile ++ "_noaudio")
        
          -- temporary file to output mono audio to
          gen_temp_audio :: IO FilePath 
          gen_temp_audio = gen_temp_file' ".wav" (basefile ++ "_mono")
          
          gen_temp_file' :: String -> FilePath -> IO FilePath
          gen_temp_file' extension fp = do
                file_exists <- doesFileExist (fp ++ extension)
                if file_exists then gen_temp_file' extension (fp ++ "_") else return (fp ++ extension)
           
          -- Deletes temporary files from disk
          cleanup :: [FilePath] -> IO()
          cleanup fs = mapM_ removeFile fs


-- (external) Reads the number of channels in a .wav file
check_channels :: FilePath -> IO (Int)
check_channels file = do
    (info, _) <- SF.readFile file :: IO (SF.Info, Maybe (BV.Buffer Double))
    return $ SF.channels info


