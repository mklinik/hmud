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

-- The bot's JID is "bot@example.com"
botUsername = "markus.klinik"
botServer = "localhost"
botPassword = "abc"
botResource = "oracle"
homepageURL = "https://github.com/mklinik/hmud"
groupchatJID = "gtf@conference.localhost"

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

type Nick2JidMap = Map String String
type Jid2PlayerMap = Map String String
type UserNameMap = (Nick2JidMap, Jid2PlayerMap)

-- TODO "markus.klinik@jabber.ccc.de/foobar" -> "Markus Klinik"
-- for now: "markus.klinik@jabber.ccc.de/foobar" -> "markus.klinik"
jid2player :: String -> String
jid2player jid = if "gtf@conference" `isPrefixOf` jid
  then (getResource jid)
  else (getUsername jid)

updateNick :: String -> String -> UserNameMap -> UserNameMap
updateNick nick jid (nicks, users) = (Map.insert nick jid $ Map.filter ((==) jid) nicks, users)

-- returns (Just newPlayerName, UserNameMap) when there was no such player
updatePlayerName :: String -> UserNameMap -> (Bool, UserNameMap)
updatePlayerName jid (nicks, users) = (isNewUser, (nicks, newUsers))
  where isNewUser = Map.notMember jid users
        newUsers = Map.insert jid (jid2player jid) users

getPlayerFromNick :: String -> UserNameMap -> Maybe String
getPlayerFromNick nick (nicks, users) = do
  jid <- Map.lookup nick nicks
  Map.lookup jid users

getPlayerFromJid :: String -> UserNameMap -> Maybe String
getPlayerFromJid jid (_, users) = Map.lookup jid users

run :: UserNameMap -> World -> XMPP ()
run userNames@(nicks, users) world = do
  -- Wait for an incoming message...
  msg <- waitForStanza (const True)
  if (isChat `conj` hasBody) msg then do
    let sender = maybe "" id (getAttr "from" msg)
        tokens = words $ maybe "" id (getMessageBody msg)
    liftIO $ putStrLn $ "got message from " ++ sender
    case dispatch (jid2player sender) tokens of
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
      RoleChange _ -> -- a user has joined
        case occJid occupant of
          Nothing -> return (userNames, world) -- anonymous users don't get a character
          Just jid ->
            let (isNewUser, newUserNames) = updatePlayerName jid $ updateNick (occNick occupant) jid userNames
              in if isNewUser
                  then do
                    player <- liftIO $ randomCharacter (jid2player jid)
                    newWorld <- liftIO $ stepWorld (putStrLn) world (insert player "The Black Unicorn")
                    sendGroupchatMessage groupchatJID ("Welcome " ++ (name player) ++ ", you are a " ++ (describe player))
                    return (newUserNames, newWorld)
                  else return (newUserNames, world)
      -- NickChange nick -> liftIO $ putStrLn $ "NickChange: " ++ nick -- TODO: track nick changes
      otherwise -> return (userNames, world)
    run newUserNames newWorld
  else do
    run userNames world
