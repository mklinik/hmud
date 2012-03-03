{-# LANGUAGE FlexibleInstances #-}
module Main where

import System.Console.Haskeline
import Control.Monad.Trans (liftIO)

import Hmud.Hmud
import Hmud.Message
import Hmud.Util
import Hmud.RealData
import Hmud.Commands
import Hmud.World

instance MonadHmud (InputT IO) where
  waitForMessage = parseMsg `fmap` getInputLine ">>> "
    where
      parseMsg = maybe MsgExit (MsgCommand "player" . words)
  sendMessage addr msg = liftIO . putStrLn $ describeMessage addr msg
  mkRandomCharacter = randomCharacter
  debugOut = liftIO . putStrLn

main :: IO ()
main = runInputT defaultSettings $
  liftIO (loadWorld "save.txt" world) >>=
  flip handleMessage (MsgPlayerEnters "player" "Markus" "a") >>=
  either return run >>=
  liftIO . saveWorld "save.txt"
