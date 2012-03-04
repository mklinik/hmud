{-# LANGUAGE FlexibleInstances #-}
module Main where

import           Network
import qualified Network.XMPP as XMPP
import           Network.XMPP (XMPP)
import qualified Network.XMPP.MUC as XMPP
import           Data.List (isPrefixOf, intercalate)
import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Monad (mapM_)
import           Control.Monad.Trans (lift)
import           Data.Maybe (isJust)
import           Control.Monad.Trans (liftIO)
import qualified Control.Concurrent.Chan as Chan
import           Control.Concurrent.Chan (Chan)
import           Control.Concurrent (forkIO)
import qualified Control.Monad.State as State
import           Control.Monad.State (StateT)
import qualified Control.Exception as Ex

-- config file handling
import qualified Data.Config.String as Config
import           Data.Config.String (Config)

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
  , xmppUseSSL :: Bool
  }

botJid :: XmppConfig -> String
botJid cfg =
  (xmppUsername cfg) ++ "@" ++ (xmppServer cfg) ++ "/" ++ (xmppResource cfg)

data XmppState = XmppState
  { xChan :: Chan IncomingMessage
  , xConfig :: XmppConfig
  }

logString :: String -> IO ()
logString s = do
  t <- getCurrentTime
  putStr (formatTime defaultTimeLocale rfc822DateFormat t)
  putStr " "
  putStrLn s

main :: IO ()
main = withSocketsDo $ do
  Just xmppConfig <- readXmppConfig "hmudrc"
  msgChan <- Chan.newChan :: IO (Chan IncomingMessage)
  let xmppState = (XmppState msgChan xmppConfig)

  -- see ssl_server.txt
  c <- if xmppUseSSL xmppConfig
         then XMPP.connectStream [("localhost", PortNumber 31337)] >>=
              flip XMPP.sendStreamHeader (xmppServer xmppConfig)
         else XMPP.openStream (xmppServer xmppConfig)

  XMPP.getStreamStart c

  XMPP.runXMPP c $ do
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

        XMPP.joinGroupchat
            (xmppGroupchatNick xmppConfig)
            (xmppGroupchatRoom xmppConfig)
            (xmppGroupchatPassword xmppConfig)

        -- stanza handlers get their own thread
        _ <- liftIO $ forkIO $ XMPP.runXMPP c $
          XMPP.addHandler (const True) (handleAllStanzas xmppState) True >>
          XMPP.addHandler (XMPP.isIq `XMPP.conj` (isJust . XMPP.xmlPath ["ping"])) handlePing True

        -- run blocking until MsgExit
        liftIO (loadWorld "save.txt" world) >>=
          \w -> State.evalStateT (run w) xmppState >>=
          liftIO . saveWorld "save.txt"

instance MonadHmud (StateT XmppState XMPP) where
  waitForMessage = do
    msgChan <- State.gets xChan
    msg <- liftIO $ Ex.catch (Chan.readChan msgChan) (\(Ex.SomeException _) -> return MsgExit)
    liftIO $ logString $ ">> " ++ show msg
    return msg
  sendMessage addr msg = do
    liftIO $ logString $ "<< " ++ addr ++ ": " ++ show msg
    lift $ XMPP.sendMessage addr $ describeMessage addr msg
  mkRandomCharacter = randomCharacter
  debugOut = liftIO . putStrLn
  saveGame f w = liftIO $ saveWorld f w
  loadGame f w = liftIO $ loadWorld f w

handleAllStanzas :: XmppState -> XMPP.StanzaHandler
handleAllStanzas state@(XmppState msgChan _) stanza =
    maybe (return ()) (XMPP.liftIO . Chan.writeChan msgChan) $ stanza2incomingMessage state stanza

stanza2incomingMessage :: XmppState -> XMPP.XMLElem -> Maybe IncomingMessage
stanza2incomingMessage (XmppState _ cfg) stanza
  | XMPP.isChat stanza = do
      sender <- XMPP.getAttr "from" stanza
      tokens <- words `fmap` (XMPP.getMessageBody stanza)
      if sender == botJid cfg
        then Nothing -- filter messages from myself
        else return $ MsgCommand sender tokens
  | XMPP.isGroupchatPresence stanza = do
      sender <- XMPP.getAttr "from" stanza
      let (presence, occupant) = XMPP.doGroupchatPresence stanza
      case presence of
        XMPP.RoleChange _ -> do
          jid <- XMPP.occJid occupant
          if jid == botJid cfg
            then Nothing -- filter presence msg from myself
            else return $ MsgPlayerEnters sender (jid2player jid) (jid2primKey jid)
        XMPP.Leave ->
          return $ MsgPlayerLeaves sender
        _ -> Nothing
  | otherwise = Nothing

handlePing :: XMPP.StanzaHandler
handlePing stanza =
  maybe (return ()) (\s -> (XMPP.liftIO $ logString "ping") >> XMPP.sendStanza s) $ constructPong stanza

constructPong :: XMPP.XMLElem -> Maybe XMPP.XMLElem
constructPong stanza = do
  idAttr <- XMPP.getAttr "id" stanza
  from   <- XMPP.getAttr "from" stanza
  to     <- XMPP.getAttr "to" stanza
  return $ XMPP.XML "iq" [("from", to), ("to", from), ("id", idAttr), ("type", "result")] []

readXmppConfig :: FilePath -> IO (Maybe XmppConfig)
readXmppConfig fileName = readXmppConfig_ `fmap` readFile fileName

readXmppConfig_ :: String -> Maybe XmppConfig
readXmppConfig_ fileContent = do
  config <- either (const Nothing) Just $ Config.parse fileContent
  username       <- Config.lookup "xmpp" "username" config
  server         <- Config.lookup "xmpp" "server" config
  password       <- Config.lookup "xmpp" "password" config
  resource       <- Config.lookup "xmpp" "resource" config
  groupchat      <- Config.lookup "xmpp" "groupchatRoom" config
  let groupchatPw = Config.lookup "xmpp" "groupchatPassword" config
  groupchatNick  <- Config.lookup "xmpp" "groupchatNick" config
  useSSL         <- Config.lookup "xmpp" "useSSL" config
  return XmppConfig
    { xmppUsername = username
    , xmppServer = server
    , xmppPassword = password
    , xmppResource = resource
    , xmppGroupchatRoom = groupchat
    , xmppGroupchatPassword = groupchatPw
    , xmppGroupchatNick = groupchatNick
    , xmppUseSSL = useSSL == "yes"
    }
