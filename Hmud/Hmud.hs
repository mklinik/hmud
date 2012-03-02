{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Hmud.Hmud where

import qualified Control.Monad.State as State
import Control.Monad.State (State)

import Hmud.Message
import Hmud.Character

class Monad m => MonadHmud m where
  waitForMessage :: m IncomingMessage
  sendMessage :: Address -> Message -> m ()
  mkRandomCharacter :: String -> Address -> String -> m Character
  debugOut :: String -> m ()

-- for testing only: maps a list of IncomingMessages to a list of outgoing Messages
type TestStateOutgoing = (Address, Message)
instance MonadHmud (State ([IncomingMessage], [TestStateOutgoing], [String])) where
  waitForMessage = do
    (ins, outs, debugs) <- State.get
    case ins of
      [] -> return MsgExit
      (m:ms) -> State.put (ms, outs, debugs) >> return m
  sendMessage recv msg = do
    (ins, outs, debugs) <- State.get
    State.put (ins, outs ++ [(recv, msg)], debugs)
  mkRandomCharacter name addr primKey =
      return Character { charName = name
                       , charRace = Human
                       , charRole = Wizard
                       , charGender = Male
                       , charLevel = 42
                       , charInventory = []
                       , charAddress = addr
                       , charId = primKey
                       }
  debugOut msg = do
    (ins, outs, debugs) <- State.get
    State.put (ins, outs, debugs ++ [msg])
