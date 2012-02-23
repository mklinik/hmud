{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Hmud.Hmud where

import qualified Control.Monad.State as State
import Control.Monad.State (State)

import Hmud.Commands
import Hmud.Message
import Hmud.Character
import Hmud.World
import Hmud.Util
import Hmud.Describable

class Monad m => MonadHmud m where
  waitForMessage :: m IncomingMessage
  sendMessage :: String -> String -> m ()
  mkRandomCharacter :: String -> Address -> m Character
  stepWorld_ :: Address -> World -> WorldAction -> m World
  debugOut :: String -> m ()

-- for testing only: maps a list of IncomingMessages to a list of outgoing Messages
instance MonadHmud (State ([IncomingMessage], [Message])) where
  waitForMessage = do
    (ins, outs) <- State.get
    case ins of
      [] -> return MsgExit
      (m:ms) -> State.put (ms, outs) >> return m
  sendMessage _ _ = return () -- already handeled in stepWorld_
  mkRandomCharacter name addr =
      return Character { charName = name
                       , charRace = Human
                       , charRole = Wizard
                       , charGender = Male
                       , charLevel = 42
                       , charInventory = []
                       , charAddress = addr
                       }
  stepWorld_ (Address sender) = stepWorld (\msg -> do
    (ins, outs) <- State.get
    State.put (ins, outs ++ [msg]))
  debugOut _ = return ()

run :: MonadHmud m => World -> m World
run world = do
  msg <- waitForMessage
  case msg of
    MsgCommand playerId tokens -> case dispatch playerId tokens of
          Nothing -> run world -- empty command
          Just a  -> do
            w2 <- (stepWorld_ playerId) world a
            run w2
    MsgPlayerEnters playerId playerName -> do
      newWorld <- do
        case findCharacter playerName world of
          Left _ -> do
              player <- mkRandomCharacter playerName playerId
              newWorld <- stepWorld_ playerId world (insert player "The Black Unicorn")
              debugOut ("Welcome " ++ (name player) ++ ", you are a " ++ (describe player))
              -- XMPP.sendGroupchatMessage groupchatJID ("Welcome " ++ (name player) ++ ", you are a " ++ (describe player))
              return newWorld
          Right _ -> return world
      run newWorld
    MsgExit -> return world
