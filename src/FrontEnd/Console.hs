{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Used otherwise as a pattern" #-}

-- | The console front-end is responsible for console I/O and
-- appropriate handling of other high-level bot interactions (menu
-- output etc).
module FrontEnd.Console
  ( run,
    Handle (..),
  )
where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified EchoBot
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

newtype Handle = Handle
  { hBotHandle :: EchoBot.Handle IO T.Text
  }

run :: Handle -> IO ()
run h = do
  TIO.putStrLn "Welcome to the echo-bot!"
  until_ (`elem` ["/quit", "/q"]) (readPrompt "echo-bot> ") (process h)

flushStr :: T.Text -> IO ()
flushStr str = TIO.putStr str >> hFlush stdout

readPrompt :: T.Text -> IO T.Text
readPrompt prompt = flushStr prompt >> TIO.getLine

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

getResponse :: Handle -> T.Text -> IO [EchoBot.Response T.Text]
getResponse h query = EchoBot.respond (hBotHandle h) $ EchoBot.MessageEvent query

handleResponse :: Handle -> [EchoBot.Response T.Text] -> IO [EchoBot.Response T.Text]
handleResponse h x = case x of
  [EchoBot.MenuResponse title options] -> handleMenu h title options
  _ -> return x

handleMenu :: Handle -> T.Text -> [(Int, EchoBot.Event T.Text)] -> IO [EchoBot.Response T.Text]
handleMenu h title options = do
  TIO.putStrLn title
  TIO.putStrLn "Enter your desired count to change it. Enter anything else to proceed without changes."
  TIO.putStrLn $ "Available repeat options: " <> T.pack (show $ map fst options)
  input <- getLine
  let newRepCount = readMaybe input :: Maybe Int
  let key = fromMaybe 0 newRepCount
  case lookup key options of
    Just (EchoBot.SetRepetitionCountEvent n) -> EchoBot.respond (hBotHandle h) $ EchoBot.SetRepetitionCountEvent n
    _ -> EchoBot.respond (hBotHandle h) $ EchoBot.MessageEvent T.empty

extractValue :: EchoBot.Response T.Text -> T.Text
extractValue (EchoBot.MessageResponse resp) = resp
extractValue (EchoBot.MenuResponse title _) = title

printResponse :: [EchoBot.Response T.Text] -> IO ()
printResponse response =
  if "" `elem` valuesList response
    then TIO.putStr T.empty
    else TIO.putStr (T.pack $ unlines $ map show response)
  where
    valuesList = map extractValue

process :: Handle -> T.Text -> IO ()
process h x = getResponse h x >>= handleResponse h >>= printResponse
