module Xmpp.Main where

import Network
import Network.XMPP
import Data.List (isPrefixOf, intercalate)

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.TestData
import Hmud.Commands

-- The bot's JID is "bot@example.com"
botUsername = "oracle"
botServer = "localhost"
botPassword = "abc"
botResource = "hmud"

stepToStdout = stepWorld putStrLn

main :: IO ()
main = withSocketsDo $
  do
    -- Connect to server...
    c <- openStream botServer
    getStreamStart c

    runXMPP c $ do
    -- ...authenticate...
    startAuth botUsername botServer botPassword botResource
    sendPresence Nothing Nothing
    -- ...and do something.

    player <- liftIO $ randomCharacter "Markus"
    npc1 <- liftIO $ randomCharacter "Martin"
    npc2 <- liftIO $ randomCharacter "Karin"
    npc3 <- liftIO $ randomCharacter "Kathy"

    w2 <- liftIO $ stepToStdout world (insert player "The Black Unicorn")
    w3 <- liftIO $ stepToStdout w2 (insert npc1 "The Black Unicorn")
    w4 <- liftIO $ stepToStdout w3 (insert npc2 "The Black Unicorn")
    w5 <- liftIO $ stepToStdout w4 (insert npc3 "town square")
    w6 <- liftIO $ stepToStdout w5 (insertItem scroll0 "ivory tower")
    w7 <- liftIO $ stepToStdout w6 (insertItem beer "The Black Unicorn")
    w8 <- liftIO $ stepToStdout w7 (insertItem scroll1 "The Black Unicorn")

    run "Markus" w8

run :: String -> World -> XMPP ()
run playerName world = do
  -- Wait for an incoming message...
  msg <- waitForStanza (isChat `conj` hasBody)
  let sender = maybe "" id (getAttr "from" msg)
      tokens = words $ maybe "" id (getMessageBody msg)
  case dispatch playerName tokens of
    Nothing -> run playerName world
    Just a  -> do
      w2 <- stepWorld (sendMessage sender) world a
      run playerName w2
