module Hmud.Message where

import Data.Char (isLower)

import Hmud.Character
import Hmud.Room
import Hmud.Item
import Hmud.Describable

-- * info messages are sent to the player only
-- * goto messages are sent to:
--  * all players of the old room (someone leaves)
--  * all players of the new room (someone enters)
-- * take, put and give messages are sent to all players of the current room
data Message =
    MsgInfo String
  | MsgGoto Room Character Room
  | MsgTake Character Item
  | MsgPut Character Item
  | MsgGive Character Item Character
  | MsgForge Character Item
  deriving (Show, Eq)

data IncomingMessage =
    MsgCommand Address [String]
  | MsgPlayerEnters Address String String
  | MsgPlayerLeaves Address
  | MsgExit
  deriving (Show, Eq)

isMsgGoto (MsgGoto _ _ _) = True
isMsgGoto _ = False

isMsgTake (MsgTake _ _) = True
isMsgTake _ = False

isMsgPut (MsgPut _ _) = True
isMsgPut _ = False

isMsgGive (MsgGive _ _ _) = True
isMsgGive _ = False

isMsgForge (MsgForge _ _) = True
isMsgForge _ = False

-- turn a message object into nice text, addressed to the receiver
describeMessage :: Address -> Message -> String
describeMessage _ (MsgInfo text) = text
describeMessage receiver (MsgGoto fromRoom char toRoom)
  | receiver == (charAddress char) = "You are now in " ++ (name toRoom) ++ ", " ++ (describe toRoom)
  | roomHasCharacterByAddress receiver fromRoom = (name char) ++ " leaves."
  | roomHasCharacterByAddress receiver toRoom   = (name char) ++ " enters."
  | otherwise = (name char) ++ " goes to " ++ (name toRoom) -- should never happen
describeMessage receiver (MsgTake char item)
  | receiver == (charAddress char) = "You take " ++ (doArticleMagic $ name item)
  | otherwise = (name char) ++ " takes " ++ (doArticleMagic $ name item)
describeMessage receiver (MsgForge char item)
  | receiver == (charAddress char) = "The world around you gets dark. All sounds seem to fade. A moment of complete darkness is followed by a bright flash. As you slowly open your eyes again, " ++ (doArticleMagic $ name item) ++ " hovers in the air before you, then floats slowly into your hands."
  | otherwise = (name char) ++ " forges " ++ (doArticleMagic $ name item)
describeMessage receiver (MsgPut char item)
  | receiver == (charAddress char) = "You put " ++ (doArticleMagic $ name item) ++ " down."
  | otherwise = (name char) ++ " puts " ++ (doArticleMagic $ name item) ++ " down."
describeMessage receiver (MsgGive giver item givee)
  | receiver == (charAddress giver) = "You give " ++ (doArticleMagic $ name item) ++ " to " ++ (name givee)
  | receiver == (charAddress givee) = (name giver) ++ " gives you " ++ (doArticleMagic $ name item)
  | otherwise = (name giver) ++ " gives " ++ (doArticleMagic $ name item) ++ " to " ++ (name givee)

-- if /name/ starts with a lower letter, add the article 'a' to it
doArticleMagic :: String -> String
doArticleMagic [] = []
doArticleMagic name@(c:cs) = if isLower c then "a " ++ name else name
