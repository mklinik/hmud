module Xmpp.Users where

import qualified Data.Map as Map
import Data.Map (Map)
import Network.XMPP
import Network.XMPP.MUC
import Data.List (isPrefixOf)

import Hmud.Util

type Nick2JidMap = Map String String
type Jid2PlayerMap = Map String String
type UserNameMap = (Nick2JidMap, Jid2PlayerMap)

jid2player :: String -> String
jid2player jid = unwords $ map capitalize $ wordsPunct $ if "gtf@conference" `isPrefixOf` jid
  then (getResource jid)
  else (getUsername jid)

updateNick :: String -> String -> UserNameMap -> UserNameMap
updateNick nick jid (nicks, users) = (Map.insert nick jid $ Map.filter ((/=) jid) nicks, users)

-- returns (True, UserNameMap) when there was no such player
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

getPlayerFromNickOrJid :: String -> UserNameMap -> Maybe String
getPlayerFromNickOrJid nickOrJid map = if "gtf@conference" `isPrefixOf` nickOrJid
  then getPlayerFromNick (getResource nickOrJid) map
  else getPlayerFromJid nickOrJid map
