{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.List
import Data.Time.Clock
import Data.Time.Calendar

import Text.Regex

import System.Directory
import System.Environment
import System.Exit
import System.Hclip (setClipboard)
import System.IO
import System.Process

type AuthorName  = String
type DateString  = String
type LogFunction = DateString -> AuthorName -> Maybe FilePath -> IO String

main :: IO ()
main =
  getCurrentTime >>= return . showGregorian . utctDay >>= \today ->
  currentGitUser >>= \curUser ->
  getArgs >>=
  (\case
    []                              -> return (gitLogDay      , today, curUser,   Nothing)
    ["--files"]                     -> return (gitLogOnlyFiles, today, curUser,   Nothing)
    [date]                          -> return (gitLogDay      ,  date, curUser,   Nothing)
    ("--files":date:author:path:_)  -> return (gitLogOnlyFiles,  date,  author, Just path)
    ("--files":date:author:_)       -> return (gitLogOnlyFiles,  date,  author,   Nothing)
    ("--files":date:_)              -> return (gitLogOnlyFiles,  date, curUser,   Nothing)
    (date:author:path:_)            -> return (gitLogDay      ,  date,  author, Just path)
    (date:author:_)                 -> return (gitLogDay      ,  date,  author,   Nothing)) >>=
    mainLog

mainLog :: (LogFunction, DateString, AuthorName, Maybe FilePath) -> IO ()
mainLog (logFunction, date, author, mb_path) =
  logFunction date author mb_path >>= \log ->
  putStr log >>
  setClipboard log >>
  putStrLn "Copied to clipboard!"

currentGitUser :: IO String
currentGitUser =
  readProcess "git" ["config", "user.name"] [] >>=
  return . head . lines

gitLogParams :: [String] -> DateString -> AuthorName -> Maybe FilePath -> IO String
gitLogParams params date author mb_path =  do
  (_, Just hOut, _, handle) <- createProcess process
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> hGetContents hOut
    _           -> return ""
  where
    process = (proc "git" params){ cwd = mb_path
                                 , std_out = CreatePipe
                                 , std_err = CreatePipe
                                 }

gitLogDay :: DateString -> AuthorName -> Maybe FilePath -> IO String
gitLogDay date author mb_path =
  gitLogParams params date author mb_path >>=
  return . formatLog
  where
    params = [
        "log"
      , "--author=" ++ author ++ ""
      , "--after='" ++ date ++ " 00:00'"
      , "--before='" ++ date ++ " 23:59'"
      , "--abbrev-commit"
      , "--no-merges"
      , "--reverse"
      , "--pretty=oneline"
      ]

gitLogOnlyFiles :: DateString -> AuthorName -> Maybe FilePath -> IO String
gitLogOnlyFiles date author mb_path =
  gitLogParams params date author mb_path >>=
  return . unlines . sort . nub . filter (/= "") . lines
  where
    params = [
        "log"
      , "--author=" ++ author ++ ""
      , "--after='" ++ date ++ " 00:00'"
      , "--no-merges"
      , "--reverse"
      , "--name-only"
      , "--pretty=format:"
      ]

formatLog :: String -> String
formatLog = unlinesHTML . map splitFeatures . lines . makeHeader . trimSpaces

unlinesHTML [] = ""
unlinesHTML (x:xs) = x ++ "<br/>\n" ++ unlinesHTML xs

makeHeader    = replaceAll "^(.{7}[^-]+) " "\\1\n  "
trimSpaces    = replaceAll " +" " "
splitFeatures = replaceAll "(;) " "\\1<br/>\n  "

replaceAll pattern sub str = subRegex re str sub
  where re = mkRegexWithOpts pattern True False
