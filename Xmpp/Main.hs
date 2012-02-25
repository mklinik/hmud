module Main where

import Network
import qualified Network.XMPP as XMPP
import Network.XMPP (XMPP)
import qualified Network.XMPP.MUC as XMPP
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (mapM_)

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.TestData
import Hmud.Commands
import Hmud.Message
import Hmud.Hmud
import Xmpp.Util

-- The bot's JID is "bot@example.com"
botUsername = "markus.klinik"
botServer = "localhost"
botPassword = "abc"
botResource = "oracle"
groupchatJID = "gtf@conference.localhost"
botJID = botUsername ++ "@" ++ botServer ++ "/" ++ botResource

main :: IO ()
main = withSocketsDo $ do
  -- Connect to server...
  c <- XMPP.openStream botServer
  XMPP.getStreamStart c

  XMPP.runXMPP c $ do
  -- ...authenticate...
  XMPP.startAuth botUsername botServer botPassword botResource
  XMPP.sendPresence (Just ("", [homepageURL, ""])) Nothing
  XMPP.handleVersion "hmud" "0.1" "Linux"
  -- ...and do something.

  XMPP.joinGroupchat "oracle" groupchatJID Nothing

  w1 <- XMPP.liftIO $ stepWorld Nothing world (insertItem scroll1 "The Black Unicorn")

  run w1
  return ()

instance MonadHmud XMPP where
  waitForMessage = waitForMessageXmpp
  sendMessage Nothing _ = return ()
  sendMessage (Just addr) msg = XMPP.sendMessage addr $ describeMessage (Just addr) msg
  mkRandomCharacter name addr = XMPP.liftIO $ randomCharacter name addr
  debugOut str = XMPP.liftIO $ putStrLn str

waitForMessageXmpp :: XMPP IncomingMessage
waitForMessageXmpp = do
  stanza <- XMPP.waitForStanza (const True)
  if (XMPP.isChat `XMPP.conj` XMPP.hasBody) stanza then do
    let sender = XMPP.getAttr "from" stanza
        tokens = words $ maybe "" id (XMPP.getMessageBody stanza)
    case sender of
      Nothing -> waitForMessageXmpp
      Just playerId | playerId == botJID -> waitForMessageXmpp
      Just playerId -> return $ MsgCommand (Just playerId) tokens
  else if XMPP.isGroupchatPresence stanza then do
    let sender = XMPP.getAttr "from" stanza
    case sender of
      Nothing -> waitForMessageXmpp
      Just playerId -> do
        let (presence, occupant) = XMPP.doGroupchatPresence stanza
        case presence of
          XMPP.RoleChange _ -> do -- a user has joined or changed nick
            case XMPP.occJid occupant of
              Nothing -> waitForMessageXmpp -- anonymous users don't get a character
              Just jid -> if (jid == botJID)
                then waitForMessageXmpp -- filter presence msg from myself
                else do
                  return $ MsgPlayerEnters (Just playerId) (jid2player jid)
          otherwise -> waitForMessageXmpp
  else waitForMessageXmpp
