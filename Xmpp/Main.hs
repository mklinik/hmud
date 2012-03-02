module Main where

import           Network
import qualified Network.XMPP as XMPP
import           Network.XMPP (XMPP)
import qualified Network.XMPP.MUC as XMPP
import           Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Monad (mapM_)
import           Data.Maybe (isJust)
import           Control.Monad.Trans (liftIO)

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

data XmppConfig = XmppConfig
  { xmppUsername :: String
  , xmppServer :: String
  , xmppPassword :: String
  , xmppResource :: String
  , xmppGroupchatRoom :: String
  , xmppGroupchatPassword :: Maybe String
  , xmppGroupchatNick :: String
  }

xmppConfig :: XmppConfig
xmppConfig = XmppConfig
  { xmppUsername = "oracle"
  , xmppServer = "localhost"
  , xmppPassword = "abc"
  , xmppResource = "oracle"
  , xmppGroupchatRoom = "gtf@conference.localhost"
  , xmppGroupchatPassword = Nothing
  , xmppGroupchatNick = "oracle"
  }

botJid :: XmppConfig -> String
botJid cfg =
  (xmppUsername cfg) ++ "@" ++ (xmppServer cfg) ++ "/" ++ (xmppResource cfg)

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
  c <- XMPP.openStream (xmppServer xmppConfig)
  -- DONE connecting

  XMPP.getStreamStart c

  XMPP.runXMPP c $ do
    -- ...authenticate...
    success <- XMPP.startAuth
        (xmppUsername xmppConfig)
        (xmppServer xmppConfig)
        (xmppPassword xmppConfig)
        (xmppResource xmppConfig)
    if success /= 0
      then
        error "Authentication not successful."
      else do
        XMPP.sendPresence (Just ("", [homepageURL, ""])) Nothing
        -- ...and do something.

        XMPP.joinGroupchat
            (xmppGroupchatNick xmppConfig)
            (xmppGroupchatRoom xmppConfig)
            (xmppGroupchatPassword xmppConfig)

        run world
        return ()

instance MonadHmud XMPP where
  waitForMessage = do
    msg <- waitForMessageXmpp
    liftIO $ logString $ ">> " ++ show msg
    return msg
  sendMessage addr msg = do
    liftIO $ logString $ "<< " ++ addr ++ ": " ++ show msg
    XMPP.sendMessage addr $ describeMessage addr msg
  mkRandomCharacter = randomCharacter
  debugOut = liftIO . putStrLn

waitForMessageXmpp :: XMPP IncomingMessage
waitForMessageXmpp = do
  stanza <- XMPP.waitForStanza (const True)
  if XMPP.isIq stanza && isJust (XMPP.xmlPath ["ping"] stanza)
    then handlePing stanza >> waitForMessageXmpp
    else maybe waitForMessageXmpp return $ stanza2incomingMessage stanza

stanza2incomingMessage :: XMPP.XMLElem -> Maybe IncomingMessage
stanza2incomingMessage stanza
  | XMPP.isChat stanza = do
      sender <- XMPP.getAttr "from" stanza
      tokens <- words `fmap` (XMPP.getMessageBody stanza)
      if sender == botJid xmppConfig
        then Nothing -- filter messages from myself
        else return $ MsgCommand sender tokens
  | XMPP.isGroupchatPresence stanza = do
      sender <- XMPP.getAttr "from" stanza
      let (presence, occupant) = XMPP.doGroupchatPresence stanza
      case presence of
        XMPP.RoleChange _ -> do
          jid <- XMPP.occJid occupant
          if jid == botJid xmppConfig
            then Nothing -- filter presence msg from myself
            else return $ MsgPlayerEnters sender (jid2player jid) (jid2primKey jid)
        XMPP.Leave ->
          return $ MsgPlayerLeaves sender
        _ -> Nothing
  | otherwise = Nothing

handlePing :: XMPP.XMLElem -> XMPP ()
handlePing stanza = do
  maybe (return ()) (XMPP.sendStanza) $ constructPong stanza

constructPong :: XMPP.XMLElem -> Maybe XMPP.XMLElem
constructPong stanza = do
  idAttr <- XMPP.getAttr "id" stanza
  from   <- XMPP.getAttr "from" stanza
  to     <- XMPP.getAttr "to" stanza
  return $ XMPP.XML "iq" [("from", to), ("to", from), ("id", idAttr), ("type", "result")] []
