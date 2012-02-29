module Main where

import Network
import qualified Network.XMPP as XMPP
import Network.XMPP (XMPP)
import qualified Network.XMPP.MUC as XMPP
import Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad (mapM_)
import Data.Maybe (isJust)

-- for logging
import Data.Time (getCurrentTime)
import System.Locale (defaultTimeLocale, rfc822DateFormat)
import Data.Time.Format (formatTime)

import Hmud.Item
import Hmud.Describable
import Hmud.Character
import Hmud.Room
import Hmud.World
import Hmud.Util
import Hmud.RealData
import Hmud.Commands
import Hmud.Message
import Hmud.Hmud
import Xmpp.Util

-- The bot's JID is "bot@example.com"
botUsername = "oracle"
botServer = "localhost"
botPassword = "abc"
botResource = "oracle"
groupchatJID = "gtf@conference." ++ botServer
groupchatPassword = Nothing
botJID = botUsername ++ "@" ++ botServer ++ "/" ++ botResource

logString :: String -> IO ()
logString s = do
  t <- getCurrentTime
  putStr (formatTime defaultTimeLocale rfc822DateFormat t)
  putStr " "
  putStrLn s

main :: IO ()
main = withSocketsDo $ do

  -- EITHER: Connect to server with SSL: you need ncat running, see ssl_server.txt
  -- s <- XMPP.connectStream [("localhost", PortNumber 31337)]
  -- c <- XMPP.sendStreamHeader s botServer
  -- OR: Connect to server without SSL
  c <- XMPP.openStream botServer
  -- DONE connecting

  XMPP.getStreamStart c

  XMPP.runXMPP c $ do
    -- ...authenticate...
    success <- XMPP.startAuth botUsername botServer botPassword botResource
    if success /= 0
      then
        error "Authentication not successful."
      else do
        XMPP.sendPresence (Just ("", [homepageURL, ""])) Nothing
        XMPP.handleVersion "hmud" "0.1" "Linux"
        -- ...and do something.

        XMPP.joinGroupchat "oracle" groupchatJID groupchatPassword

        run world
        return ()

instance MonadHmud XMPP where
  waitForMessage = waitForMessageXmpp
  sendMessage Nothing _ = return ()
  sendMessage (Just addr) msg = do
    XMPP.liftIO $ logString $ "<< " ++ addr ++ ": " ++ show msg
    XMPP.sendMessage addr $  describeMessage (Just addr) msg
  mkRandomCharacter name addr primKey = XMPP.liftIO $ randomCharacter name addr primKey
  debugOut str = XMPP.liftIO $ putStrLn str

waitForMessageXmpp :: XMPP IncomingMessage
waitForMessageXmpp = do
  stanza <- XMPP.waitForStanza (const True)
  if (XMPP.isChat `XMPP.conj` XMPP.hasBody) stanza
    then do
      let sender = XMPP.getAttr "from" stanza
          tokens = words $ maybe "" id (XMPP.getMessageBody stanza)
      case sender of
        Nothing -> waitForMessageXmpp
        Just playerAddr | playerAddr == botJID -> waitForMessageXmpp
        Just playerAddr -> do
          XMPP.liftIO $ logString $ ">> " ++ playerAddr ++ ": " ++ (unwords tokens)
          return $ MsgCommand (Just playerAddr) tokens
    else if XMPP.isGroupchatPresence stanza
      then do
        let sender = XMPP.getAttr "from" stanza
        case sender of
          Nothing -> waitForMessageXmpp
          playerAddr -> do
            let (presence, occupant) = XMPP.doGroupchatPresence stanza
            case presence of
              XMPP.RoleChange _ -> do -- a user has joined or changed nick
                case XMPP.occJid occupant of
                  Nothing -> waitForMessageXmpp -- anonymous users don't get a character
                  Just jid -> if (jid == botJID)
                    then waitForMessageXmpp -- filter presence msg from myself
                    else do
                      let msg = MsgPlayerEnters playerAddr (jid2player jid) (jid2primKey jid)
                      XMPP.liftIO $ logString $ ">> " ++ show msg
                      return msg
              otherwise -> waitForMessageXmpp
      else if XMPP.isIq stanza && isJust (XMPP.xmlPath ["ping"] stanza)
        then do
          let idAttr = maybe "" id (XMPP.getAttr "id" stanza)
              from = maybe "" id (XMPP.getAttr "from" stanza)
              to =  maybe "" id (XMPP.getAttr "to" stanza)
              pong = XMPP.XML "iq" [("from", to), ("to", from), ("id", idAttr), ("type", "result")] []
          XMPP.sendStanza pong
          waitForMessageXmpp
        else
          waitForMessageXmpp
