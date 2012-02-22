module Hmud.Hmud where

import Hmud.Commands
import Hmud.Message
import Hmud.Character
import Hmud.World
import Hmud.Util

class Monad m => MonadHmud m where
  waitForMessage :: m IncomingMessage
  sendMessage :: String -> String -> m ()
  liftIO :: IO a -> m a
  stepWorld_ :: Address -> World -> WorldAction -> m World

run :: MonadHmud m => World -> m ()
run world = do
  msg <- waitForMessage
  case msg of
    MsgCommand playerId tokens -> case dispatch playerId tokens of
          Nothing -> run world
          Just a  -> do
            w2 <- (stepWorld_ playerId) world a
            run w2
    -- MsgPlayerEnters playerId playerName -> undefined -- TODO
    MsgPlayerEnters playerId playerName -> do
      newWorld <-
        case findCharacter playerName world of
          Left _ -> do
              player <- liftIO $ randomCharacter playerName playerId
              newWorld <- liftIO $ stepToStdout world (insert player "The Black Unicorn")
              -- XMPP.sendGroupchatMessage groupchatJID ("Welcome " ++ (name player) ++ ", you are a " ++ (describe player))
              return newWorld
          Right _ -> return world
      run newWorld
