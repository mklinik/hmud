module Main where

import Network
import Network.XMPP
import Network.XMPP.MUC
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map
import Data.Map (Map)

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.TestData
import Hmud.Commands
import Xmpp.Users

-- The bot's JID is "bot@example.com"
botUsername = "markus.klinik"
botServer = "localhost"
botPassword = "abc"
botResource = "oracle"
homepageURL = "https://github.com/mklinik/hmud"
groupchatJID = "gtf@conference.localhost"
botJID = botUsername ++ "@" ++ botServer ++ "/" ++ botResource

stepToStdout = stepWorld putStrLn

main :: IO ()
main = withSocketsDo $ do
  -- Connect to server...
  c <- openStream botServer
  getStreamStart c

  runXMPP c $ do
  -- ...authenticate...
  startAuth botUsername botServer botPassword botResource
  sendPresence (Just ("", [homepageURL, ""])) Nothing
  handleVersion "hmud" "0.1" "Linux"
  -- ...and do something.

  joinGroupchat "oracle" groupchatJID Nothing

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

  run (Map.empty, Map.empty) w8

run :: UserNameMap -> World -> XMPP ()
run userNames@(nicks, users) world = do
  -- Wait for an incoming message...
  msg <- waitForStanza (const True)
  if (isChat `conj` hasBody) msg then do
    let sender = maybe "" id (getAttr "from" msg)
        tokens = words $ maybe "" id (getMessageBody msg)
    liftIO $ putStrLn $ "got message from " ++ sender
    case dispatch (getPlayerFromNickOrJid sender userNames) tokens of
      Nothing -> run userNames world
      Just a  -> do
        w2 <- stepWorld (sendMessage sender) world a
        run userNames w2
  else if isGroupchatPresence msg then do
    -- * maintain nick -> jid mapping
    -- * maintain jid -> player name mapping
    -- * greet users as they join (any RoleChange must do for us)
    liftIO $ putStrLn $ "got groupchat presence from: " ++ (maybe "" id (getAttr "from" msg))
    let (presence, occupant) = doGroupchatPresence msg
    (newUserNames, newWorld) <- case presence of
      RoleChange _ -> -- a user has joined or changed nick
        case occJid occupant of
          Nothing -> return (userNames, world) -- anonymous users don't get a character
          Just jid -> if (jid == botJID)
            then return (userNames, world) -- filter presence msg from myself
            else
              let (isNewUser, newUserNames) = updatePlayerName jid $ updateNick (occNick occupant) jid userNames
                in if isNewUser
                    then do
                      player <- liftIO $ randomCharacter (jid2player jid)
                      newWorld <- liftIO $ stepWorld (putStrLn) world (insert player "The Black Unicorn")
                      sendGroupchatMessage groupchatJID ("Welcome " ++ (name player) ++ ", you are a " ++ (describe player))
                      return (newUserNames, newWorld)
                    else return (newUserNames, world)
      otherwise -> return (userNames, world)
    run newUserNames newWorld
  else do
    run userNames world
