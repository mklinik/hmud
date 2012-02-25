module Xmpp.Util where

import qualified Network.XMPP as XMPP
import Data.List (isPrefixOf)

import Hmud.Util

jid2player :: String -> String
jid2player jid = unwords $ map capitalize $ wordsPunct $ if "gtf@conference" `isPrefixOf` jid
  then (XMPP.getResource jid)
    else (XMPP.getUsername jid)

