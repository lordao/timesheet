{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Data.Time.Clock
import Data.Time.Calendar

import Text.Regex

import System.Directory
import System.Environment
import System.Exit
import System.Hclip (setClipboard)
import System.IO
import System.Process

type AuthorName = String
type DateString = String

main :: IO ()
main =
  getCurrentTime >>= return . showGregorian . utctDay >>= \today ->
  currentGitUser >>= \curUser ->
  getArgs >>=
  (\case
    []     -> return (today, curUser, Nothing)
    [date] -> return (date, curUser, Nothing)
    (date:author:path:_) -> return (date, author, Just path)
    (date:author:_) -> return (date, author, Nothing)) >>=
  uncurry3 gitLog >>=
  return . formatLog >>= \log ->
  setClipboard log >>
  putStrLn "Copied to clipboard!"

uncurry3 f (a, b, c) = f a b c


guard True  m = m >> return True
guard False _ = return False

currentGitUser :: IO String
currentGitUser =
  readProcess "git" ["config", "user.name"] [] >>=
  return . head . lines

gitLog :: DateString -> AuthorName -> Maybe FilePath -> IO String
gitLog date author path = do
  (_, Just hOut, _, handle) <- createProcess process
  exitCode <- waitForProcess handle
  case exitCode of
    ExitSuccess -> hGetContents hOut
    _           -> return ""
  where
    process = (proc "git" params){ cwd = path
                                 , std_out = CreatePipe
                                 , std_err = CreatePipe
                                 }
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

formatLog :: String -> String
formatLog = unlinesHTML . map splitFeatures . lines . makeHeader . trimSpaces

unlinesHTML [] = ""
unlinesHTML (x:xs) = x ++ "<br/>\n" ++ unlinesHTML xs

makeHeader    = replaceAll "^(.{7}[^-]+) " "\\1\n  "
trimSpaces    = replaceAll " +" " "
splitFeatures = replaceAll "(;) " "\\1<br/>\n  "

replaceAll pattern sub str = subRegex re str sub
  where re = mkRegexWithOpts pattern True False
