{-# LANGUAGE OverloadedStrings #-}

-- | A module to provide a configuration reader for other modules.
module Config
  ( getBotConfig,
    getLoggerConfig,
    getFrontEndType,
  )
where

import qualified ConfigurationTypes
import qualified Data.Text as T
import EchoBot (Config (confHelpReply, confRepeatReply, confRepetitionCount))
import qualified EchoBot
import qualified Logger
import Logger.Impl (Config (confFileHandle, confMinLevel))
import qualified Logger.Impl
import qualified System.IO

-- | Gets the bot config. In any case it can provide reasonable
-- default values.
getBotConfig :: IO EchoBot.Config
getBotConfig =
  return
    EchoBot.Config
      { confRepeatReply = "Your repeat count is {count} now. What count would you like to set?",
        confHelpReply = T.empty,
        confRepetitionCount = 1
      }

getLoggerConfig :: IO Logger.Impl.Config
getLoggerConfig =
  return
    Logger.Impl.Config
      { confFileHandle = System.IO.stderr,
        confMinLevel = Logger.Info
      }

getFrontEndType :: IO ConfigurationTypes.FrontEndType
getFrontEndType = return ConfigurationTypes.ConsoleFrontEnd
