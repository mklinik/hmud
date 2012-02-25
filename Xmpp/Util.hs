module Xmpp.Util where

import qualified Network.XMPP as XMPP
import Data.List (isPrefixOf)

import Hmud.Util

jid2player :: String -> String
jid2player jid = unwords $ map capitalize $ wordsPunct $ if "gtf@conference" `isPrefixOf` jid
  then (XMPP.getResource jid)
  else (XMPP.getUsername jid)

-- /jid/ must be a private jabber ID, not the groupchat ID
-- jid2primKey must turn a jabber ID into a primary key for the player's
-- character. We use the jabber ID without resource, in order for players to
-- control their character from different jabber clients.
jid2primKey :: String -> String
jid2primKey = XMPP.getBareJid
