{-# LANGUAGE LambdaCase #-}
module Parser where

import Types (Command(..))
import Options.Applicative (Parser, strArgument, many, subparser, command, execParserPure, fullDesc,
                            info, getParseResult, prefs, metavar, showHelpOnEmpty)

parse :: String -> Maybe Command
parse input =
  getParseResult $ execParserPure (prefs showHelpOnEmpty) (info parseImpl fullDesc) (words input)

parseArgument :: Parser String
parseArgument = strArgument (metavar "ARGUMENT")

parseArg :: String -> Parser Command
parseArg = \case
  "cd"                         -> fmap Cd parseArgument
  "ls"                         -> fmap Ls parseArgument
  "create-folder"              -> fmap CreateFolder parseArgument
  "create-file"                -> fmap CreateFile parseArgument
  "cat"                        -> fmap Cat parseArgument
  "remove"                     -> fmap Remove parseArgument
  "write-file"                 -> fmap WriteFile parseArgument <*> many parseArgument
  "find-file"                  -> fmap FindFile parseArgument
  "information"                -> fmap Information parseArgument
  _                            -> fmap Ls parseArgument

parseImpl :: Parser Command
parseImpl =
  subparser $ command "help" (info (pure Help) fullDesc)
    <> command "cd" (info (parseArg "cd") fullDesc)
    <> command "ls" (info (parseArg "ls") fullDesc)
    <> command "dir" (info (pure Dir) fullDesc)
    <> command "create-folder" (info (parseArg "create-folder") fullDesc)
    <> command "create-file" (info (parseArg "create-file") fullDesc)
    <> command "cat" (info (parseArg "cat") fullDesc)
    <> command "remove" (info (parseArg "remove") fullDesc)
    <> command "write-file" (info (parseArg "write-file") fullDesc)
    <> command "find-file" (info (parseArg "find-file") fullDesc)
    <> command "information" (info (parseArg "information") fullDesc)
    <> command "exit" (info (pure Exit) fullDesc)
