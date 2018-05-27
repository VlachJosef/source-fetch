{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module SourceFetch.Completion
  ( executeCmd
  ) where

import           Control.Applicative                      ((<|>))
import           Control.Monad.Trans.Class                (lift)
import           Control.Monad.Trans.Identity             (IdentityT)
import           Data.List                                (isPrefixOf)
import qualified Data.Text                                as Text
import           SourceFetch.Init                         (execInit)
import           SourceFetch.Status                       (doStatus)
import           System.Console.Haskeline                 (Completion (..), InputT, Settings, defaultSettings,
                                                           getInputLine, runInputT, setComplete)
import           System.Console.Haskeline.Completion      (CompletionFunc)
import           Text.Parsec                              (ParseError, parse, spaces, try)
import           Text.Parsec.Text                         (Parser)
import           Text.ParserCombinators.Parsec.Char       (alphaNum, string)
import           Text.ParserCombinators.Parsec.Combinator (anyToken, eof, many1)

data ClientCmd
  = Fetch !Text.Text
  | Init
  | Status
  deriving (Eq, Show)

data Cmd
  = Cmd !ClientCmd
  | InvalidCmd !Text.Text !ParseError
  | ExitCmd
  | EmptyCmd
  | UnknownCmd !Text.Text
  deriving (Eq, Show)

data CommandInfo = CommandInfo
  { command    :: !String
  , takeParams :: !Bool
  } deriving Show

data CompletionExpect
    = TopLevel !String
    | NoCompletion
    deriving (Show, Eq)

type RuntimeState = IdentityT IO

settings :: CompletionFunc RuntimeState -> Settings RuntimeState
settings c = setComplete c defaultSettings

executeCmd :: IdentityT IO ()
executeCmd = runInputT (settings comp) loop
    where
       loop :: InputT RuntimeState ()
       loop =  do
           minput <- getInputLine ">>> "
           case minput of
               Nothing    -> lift $ pure ()
               Just input -> lift . lift $ execCommand (parseCommand (Text.pack input))

execCommand :: Cmd -> IO ()
execCommand = \case
  Cmd clientCmd -> execClientCmd clientCmd
  InvalidCmd a parseError -> putStrLn "Parse error"
  ExitCmd -> putStrLn "Exit"
  EmptyCmd -> putStrLn "Empty"
  UnknownCmd unknown -> putStrLn "UnknownCmd"

execClientCmd :: ClientCmd -> IO ()
execClientCmd = \case
  Fetch disconnect -> putStrLn "TODO Fetch"
  Init -> execInit
  Status -> doStatus

parseCommand :: Text.Text -> Cmd
parseCommand s = case parse pCmd "launch-command" (Text.strip s) of
    Left errors -> InvalidCmd s errors
    Right cmd   -> cmd

pCmd :: Parser Cmd
pCmd
   =  pClient
  <|> pExit
  <|> pEmpty
  <|> pUnknown

pClient :: Parser Cmd
pClient = Cmd <$> pClientCmd

pClientCmd :: Parser ClientCmd
pClientCmd
   =  pClientFetch
  <|> pClientInit
  <|> pStatus

pClientFetch, pClientInit, pStatus :: Parser ClientCmd
pClientFetch = Fetch . Text.pack <$> string "fetch"
pClientInit  = Init              <$  string "init"
pStatus      = Status            <$  string "status"

pExit, pEmpty, pUnknown :: Parser Cmd
pExit    = ExitCmd  <$ string "exit"
pEmpty   = EmptyCmd <$ spaces <* eof
pUnknown = UnknownCmd . Text.pack <$> many1 anyToken

comp :: CompletionFunc RuntimeState
comp (onLeft, _) = let

  filterCmds :: String -> [CommandInfo] -> [CommandInfo]
  filterCmds pref = filter (isPrefixOf pref . command)

  rOnLeft :: String
  rOnLeft = reverse onLeft

  completions :: ([CommandInfo], String)
  completions =
   case parse pCompletionExpect "autocomplete" (Text.stripStart . Text.pack $ rOnLeft) of
    Left _      -> ([], "")
    Right cmd   -> case cmd of
      NoCompletion  -> ([], "")
      TopLevel pref -> (filterCmds pref firstOrderCommands, "")

  in do

  let (commandInfos, prefix) = completions

  pure (reverse prefix, (\CommandInfo{..} -> Completion command command takeParams) <$> commandInfos)

firstOrderCommands :: [CommandInfo]
firstOrderCommands = terminalCommands ++ nonTerminalCommands
  where
    terminalCommands, nonTerminalCommands :: [CommandInfo]
    terminalCommands    = (`CommandInfo` False) <$> ["exit"]
    nonTerminalCommands = (`CommandInfo` True)  <$> ["init", "fetch", "status"]

pAlphaNumOrNothing :: Parser String
pAlphaNumOrNothing = (many1 alphaNum <|> pure "") <* eof

pTopLevel, pNoCompletion :: Parser CompletionExpect
pTopLevel          = TopLevel <$> pAlphaNumOrNothing
pNoCompletion      = pure NoCompletion

pCompletionExpect :: Parser CompletionExpect
pCompletionExpect
  =   try pTopLevel
  <|> pNoCompletion
