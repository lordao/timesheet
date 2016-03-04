module Timesheet where

import System.Environment
import System.Exit
import System.IO
import System.Process

type AuthorName = String
type DateString = String

main :: IO ()
main = undefined

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
