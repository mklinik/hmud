{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Hmud.Hmud where

import qualified Control.Monad.State as State
import Control.Monad.State (State)
import System.IO (hFlush, stdout)

import Hmud.Message
import Hmud.Character
import Hmud.World
import Hmud.Util
import Hmud.Describable

class Monad m => MonadHmud m where
  waitForMessage :: m IncomingMessage
  sendMessage :: Address -> Message -> m ()
  mkRandomCharacter :: String -> Address -> m Character
  debugOut :: String -> m ()

-- for testing only: maps a list of IncomingMessages to a list of outgoing Messages
instance MonadHmud (State ([IncomingMessage], [Message])) where
  waitForMessage = do
    (ins, outs) <- State.get
    case ins of
      [] -> return MsgExit
      (m:ms) -> State.put (ms, outs) >> return m
  sendMessage _ msg = do
    (ins, outs) <- State.get
    State.put (ins, outs ++ [msg])
  mkRandomCharacter name addr =
      return Character { charName = name
                       , charRace = Human
                       , charRole = Wizard
                       , charGender = Male
                       , charLevel = 42
                       , charInventory = []
                       , charAddress = addr
                       }
  debugOut _ = return ()

instance MonadHmud IO where
  waitForMessage = do
    putStr ">>> "
    hFlush stdout
    tokens <- fmap words getLine
    case tokens of
      [] -> return MsgExit
      otherwise -> return $ MsgCommand (Just "player") tokens
  sendMessage addr msg = putStrLn $ describeMessage addr msg
  mkRandomCharacter name addr = randomCharacter name addr
  debugOut = putStrLn
